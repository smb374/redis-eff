{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Parsing (
    -- * Effect
    Parsing,

    -- ** Data
    ProtocolData (..),
    Command (..),
    SetCondition (..),
    ExpireTime (..),
    ParseError (..),

    -- ** Actions
    encodeProtocol,
    encodeCommand,
    expire2unix,
    parseProtocol,
    parseCommand,

    -- ** Handler
    runParsing,
) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (toUpper)
import Data.UnixTime (UnixTime (UnixTime), addUnixDiffTime, microSecondsToUnixDiffTime)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static
import Effectful.TH (makeEffect)
import Effects.DataIO
import Effects.Timer
import Text.Printf (printf)
import Utils

-- Data
data ProtocolData
    = SimpleString ByteString
    | SimpleError ByteString
    | IntegerNumber Integer
    | BulkString Int ByteString
    | ArrayType Int (Vector ProtocolData)
    | Null
    | Boolean Bool
    | DoubleNumber Double
    | BigNumber Integer
    | BulkError Int ByteString
    | VerbatimString Int ByteString ByteString
    | MapType Int (Vector (ProtocolData, ProtocolData))
    | SetType Int (Vector ProtocolData)
    | Push Int (Vector ProtocolData)
    deriving (Show, Eq)

data Command
    = PING
    | ECHO ByteString
    | GET ByteString
    | SET ByteString ByteString (Maybe SetCondition) Bool (Maybe ExpireTime)
    | INFO (Vector ByteString)
    deriving (Show, Eq)

data SetCondition = NX | XX deriving (Show, Eq)
data ExpireTime = EX Integer | PX Integer | EXAT Integer | PXAT Integer | KEEPTTL deriving (Show, Eq)

-- Encoders
encodeCommand :: Command -> ByteString
encodeCommand PING = encodeProtocol $ ArrayType 1 $ V.singleton (BulkString 4 "PING")
encodeCommand (ECHO s) = encodeProtocol $ ArrayType 2 $ V.fromList [BulkString 4 "ECHO", BulkString (C.length s) s]
encodeCommand (GET s) = encodeProtocol $ ArrayType 2 $ V.fromList [BulkString 3 "GET", BulkString (C.length s) s]
encodeCommand (SET k v mcond getOld mexpr) = encodeProtocol $ ArrayType (V.length dat) dat
  where
    cmd = Just $ BulkString 3 "SET"
    ks = Just $ BulkString (C.length k) k
    vs = Just $ BulkString (C.length v) v
    conds = case mcond of
        Just NX -> Just $ BulkString 2 "NX"
        Just XX -> Just $ BulkString 2 "XX"
        Nothing -> Nothing
    gets = if' getOld (Just $ BulkString 3 "GET") Nothing
    exprs = case mexpr of
        Just (EX t) -> [Just (BulkString 2 "EX"), Just (BulkString (length (show t)) (C.pack (show t)))]
        Just (PX t) -> [Just (BulkString 2 "PX"), Just (BulkString (length (show t)) (C.pack (show t)))]
        Just (EXAT t) -> [Just (BulkString 4 "EXAT"), Just (BulkString (length (show t)) (C.pack (show t)))]
        Just (PXAT t) -> [Just (BulkString 4 "PXAT"), Just (BulkString (length (show t)) (C.pack (show t)))]
        Just KEEPTTL -> [Just (BulkString 7 "KEEPTTL")]
        Nothing -> [Nothing]
    dat = V.catMaybes $ V.fromList $ cmd : ks : vs : conds : gets : exprs
encodeCommand _ = C.empty

encodeProtocol :: ProtocolData -> ByteString
encodeProtocol (SimpleString msg) = C.concat ["+", msg, "\r\n"]
encodeProtocol (SimpleError msg) = C.concat ["-", msg, "\r\n"]
encodeProtocol (IntegerNumber num) =
    C.concat [":", C.pack (show num), "\r\n"]
encodeProtocol (BulkString size msg) =
    C.concat ["$", C.pack (show size), "\r\n", msg, "\r\n"]
encodeProtocol (ArrayType size arr) =
    let header = C.concat ["*", C.pack (show size), "\r\n"]
        body = V.map encodeProtocol arr
     in C.concat (header : V.toList body)
encodeProtocol Null = "_\r\n"
encodeProtocol (Boolean val) = C.concat ["#", if' val "t" "f", "\r\n"]
encodeProtocol (DoubleNumber num) = C.concat [",", C.pack (show num), "\r\n"]
encodeProtocol (BigNumber num) = C.concat ["(", C.pack (show num), "\r\n"]
encodeProtocol (BulkError size msg) =
    C.concat ["!", C.pack (show size), "\r\n", msg, "\r\n"]
encodeProtocol (VerbatimString size encode msg) =
    C.concat
        [ "="
        , C.pack (show (size + 4))
        , "\r\n"
        , C.take 3 encode
        , ":"
        , msg
        , "\r\n"
        ]
encodeProtocol (MapType size arr) =
    let header = C.concat ["%", C.pack (show size), "\r\n"]
        body =
            V.map
                (\(k, v) -> C.append (encodeProtocol k) (encodeProtocol v))
                arr
     in C.concat (header : V.toList body)
encodeProtocol (SetType size arr) =
    let header = C.concat ["~", C.pack (show size), "\r\n"]
        body = V.map encodeProtocol arr
     in C.concat (header : V.toList body)
encodeProtocol (Push size arr) =
    let header = C.concat [">", C.pack (show size), "\r\n"]
        body = V.map encodeProtocol arr
     in C.concat (header : V.toList body)

expire2unix :: Timer :> es => ExpireTime -> Eff es (Maybe UnixTime)
expire2unix expr = do
    ctime <- getTime
    let res = case expr of
            EX t -> Just (ctime, s2us t)
            PX t -> Just (ctime, ms2us t)
            EXAT t -> Just (UnixTime 0 0, s2us t)
            PXAT t -> Just (UnixTime 0 0, ms2us t)
            KEEPTTL -> Nothing
    return $ case res of
        Just (base, diff) -> Just $ addUnixDiffTime base (microSecondsToUnixDiffTime diff)
        Nothing -> Nothing

-- Error
data ParseError
    = EmptyInput
    | NotEnoughData
    | ParserFailed ByteString [String] String
    | ValidationFailed String String
    deriving (Eq)

instance Show ParseError where
    show EmptyInput = "Empty input data"
    show NotEnoughData = "Not enough data"
    show (ParserFailed _ ctxs msg) =
        printf "Parser failed: %s, contexts = %s" msg (show ctxs)
    show (ValidationFailed expect got) =
        printf "Validation failed: expect [%s], got [%s]" expect got

data Parsing :: Effect where
    ParseProtocol :: ByteString -> Parsing m (Maybe ByteString, ProtocolData)
    ParseCommand :: ByteString -> Parsing m (Maybe ByteString, Command)
type instance DispatchOf Parsing = Dynamic

makeEffect ''Parsing

parseStep :: (DataIO :> es, Error ParseError :> es) => Parser a -> ByteString -> Eff es (Maybe ByteString, a)
parseStep parser bs = handleResult $ AC.parse parser bs
  where
    handleResult r = case r of
        Done rest result ->
            return $
                if C.null rest
                    then (Nothing, result)
                    else (Just rest, result)
        Partial cont -> fetchMoreData cont
        Fail rest ctxs msg -> throwError $ ParserFailed rest ctxs msg
    fetchMoreData cont = do
        result <- readBytes 1024
        case result of
            Just s -> handleResult (cont s)
            Nothing -> throwError NotEnoughData

runParsing :: (DataIO :> es, Error ParseError :> es) => Eff (Parsing : es) a -> Eff es a
runParsing = interpret $ \_ -> \case
    ParseProtocol bs ->
        if C.null bs
            then throwError EmptyInput
            else parseStep parseProtocolData bs
    ParseCommand bs ->
        if C.null bs
            then throwError EmptyInput
            else parseCmd bs

-- Command parser
parseCmd :: (DataIO :> es, Error ParseError :> es) => ByteString -> Eff es (Maybe ByteString, Command)
parseCmd bs = do
    (rest, pdata) <- parseStep parseProtocolData bs
    cmd <- validate2command pdata
    return (rest, cmd)

validate2command :: Error ParseError :> es => ProtocolData -> Eff es Command
validate2command proto = case proto of
    ArrayType _ v
        | V.length v >= 1 -> case V.head v of
            BulkString _ cmdName -> case C.unpack $ C.map toUpper cmdName of
                "PING" -> return PING
                "ECHO" -> case v !? 1 of
                    Just (BulkString _ arg) -> return $ ECHO arg
                    x -> throwError $ ValidationFailed "ECHO <msg>" (show x)
                "GET" -> case v !? 1 of
                    Just (BulkString _ arg) -> return $ GET arg
                    x -> throwError $ ValidationFailed "GET <key>" (show x)
                "SET" -> case V.tail v of
                    args
                        | V.null args -> throwError $ ValidationFailed "Arguments of SET" "Empty arguments"
                        | otherwise -> validate2set args
                "INFO" -> case V.tail v of
                    args
                        | V.null args -> throwError $ ValidationFailed "Arguments of INFO" "Empty arguments"
                        | otherwise -> validate2info args
                x -> throwError $ ValidationFailed "Known command" x
            x -> throwError $ ValidationFailed "Bulk string" (show x)
        | otherwise -> throwError $ ValidationFailed "Non-empty array" (show proto)
    _ -> throwError $ ValidationFailed "Array type data" (show proto)

validate2set :: Error ParseError :> es => Vector ProtocolData -> Eff es Command
validate2set args = do
    key <- getBulkString 0 "SET <key>" args
    value <- getBulkString 1 "SET <key> <value>" args
    (cond, get, expire) <- validateSetOptions (V.drop 2 args)
    return $ SET key value cond get expire

validateSetOptions :: Error ParseError :> es => Vector ProtocolData -> Eff es (Maybe SetCondition, Bool, Maybe ExpireTime)
validateSetOptions args = step (Nothing, False, Nothing) len
  where
    len = V.length args
    step x 0 = return x
    step (cond, get, expire) n =
        let idx = len - n
         in case args V.! idx of
                BulkString _ "NX" -> step (Just NX, get, expire) (n - 1)
                BulkString _ "XX" -> step (Just XX, get, expire) (n - 1)
                BulkString _ "GET" -> step (cond, True, expire) (n - 1)
                BulkString _ "KEEPTTL" -> step (cond, get, Just KEEPTTL) (n - 1)
                BulkString _ s
                    | s == "EX" || s == "PX" || s == "EXAT" || s == "PXAT" -> case args V.!? (idx + 1) of
                        Just (BulkString _ ts) -> case C.readInteger ts of
                            Just (t, rest) | C.null rest -> case s of
                                "EX" -> step (cond, get, Just $ EX t) (n - 2)
                                "PX" -> step (cond, get, Just $ PX t) (n - 2)
                                "EXAT" -> step (cond, get, Just $ EXAT t) (n - 2)
                                "PXAT" -> step (cond, get, Just $ PXAT t) (n - 2)
                                _ -> undefined -- Impossible case
                            other -> throwError $ ValidationFailed "Valid timestamp" (show other)
                        other -> throwError $ ValidationFailed ("Timestamp after " <> C.unpack s) (show other)
                other -> throwError $ ValidationFailed "Valid SET option" (show other)

getBulkString :: Error ParseError :> es => Int -> String -> Vector ProtocolData -> Eff es ByteString
getBulkString idx expected args = case args !? idx of
    Just (BulkString _ s) -> return s
    x -> throwError $ ValidationFailed expected (show x)

validate2info :: Error ParseError :> es => Vector ProtocolData -> Eff es Command
validate2info args = do
    args' <-
        mapM
            ( \x -> case x of
                BulkString _ s -> return s
                _ -> throwError $ ValidationFailed "Bulk string" (show x)
            )
            $ V.toList args
    return $ INFO $ V.fromList args'

-- Protocol parser
parseProtocolData :: Parser ProtocolData
parseProtocolData = do
    c <- peekChar'
    case c of
        '+' -> parseSimpleString
        '-' -> parseSimpleError
        ':' -> parseInteger
        '$' -> parseBulkString
        '*' -> parseArray
        '_' -> parseNull
        '#' -> parseBoolean
        ',' -> parseDouble
        '(' -> parseBigNumber
        '!' -> parseBulkError
        '=' -> parseVerbatim
        '%' -> parseMap
        '~' -> parseSet
        '>' -> parsePush
        _ -> fail $ printf "Parser not implemented for data type '%c'" c

takeTillCR :: Parser ByteString
takeTillCR = do
    dat <- AC.takeWhile (/= '\r')
    endOfLine
    return dat

parseBulk :: Parser (Int, ByteString)
parseBulk = do
    size <- decimal
    endOfLine
    dat <- AC.take size
    endOfLine
    return (size, dat)

parseBulkFull :: Parser (Int, ByteString)
parseBulkFull = char '$' >> parseBulk

parseDecimal :: Parser Integer
parseDecimal = do
    n <- AC.signed decimal
    endOfLine
    return n

parseArrayLike :: Parser (Int, Vector ProtocolData)
parseArrayLike = do
    size <- decimal
    endOfLine
    elements <- AC.count size parseProtocolData
    return (size, V.fromList elements)

parseSimpleString :: Parser ProtocolData
parseSimpleString = SimpleString <$> (char '+' >> takeTillCR)

parseSimpleError :: Parser ProtocolData
parseSimpleError = SimpleError <$> (char '-' >> takeTillCR)

parseInteger :: Parser ProtocolData
parseInteger = IntegerNumber <$> (char ':' >> parseDecimal)

parseBulkString :: Parser ProtocolData
parseBulkString = uncurry BulkString <$> parseBulkFull

parseArray :: Parser ProtocolData
parseArray = uncurry ArrayType <$> (char '*' >> parseArrayLike)

parseNull :: Parser ProtocolData
parseNull = do
    _ <- char '_'
    endOfLine
    return Null

parseBoolean :: Parser ProtocolData
parseBoolean = do
    _ <- char '#'
    res <-
        anyChar >>= \c -> case c of
            't' -> return True
            'f' -> return False
            _ -> fail $ printf "Invalid input for boolean data: '%c'" c
    endOfLine
    return $ Boolean res

parseDouble :: Parser ProtocolData
parseDouble = do
    _ <- char ','
    res <- strDouble <|> AC.signed double
    endOfLine
    return $ DoubleNumber res
  where
    strDouble = do
        res <- string "inf" <|> string "-inf" <|> string "nan"
        case res of
            "inf" -> return (1.0 / 0.0)
            "-inf" -> return $ negate (1.0 / 0.0)
            "nan" -> return (0.0 / 0.0)
            _ -> fail "Impossible case branch."

parseBigNumber :: Parser ProtocolData
parseBigNumber = BigNumber <$> (char '(' >> parseDecimal)

parseBulkError :: Parser ProtocolData
parseBulkError = uncurry BulkError <$> (char '!' >> parseBulk)

parseVerbatim :: Parser ProtocolData
parseVerbatim = do
    _ <- char '='
    size <- decimal
    endOfLine
    enc <- AC.take 3
    _ <- char ':'
    dat <- AC.take (size - 4)
    endOfLine
    return $ VerbatimString size enc dat

parseMap :: Parser ProtocolData
parseMap = do
    _ <- char '%'
    size <- decimal
    endOfLine
    elements <-
        AC.count
            size
            ((,) <$> parseProtocolData <*> parseProtocolData)
    return $ MapType size (V.fromList elements)

parseSet :: Parser ProtocolData
parseSet = uncurry SetType <$> (char '~' >> parseArrayLike)

parsePush :: Parser ProtocolData
parsePush = uncurry Push <$> (char '>' >> parseArrayLike)
