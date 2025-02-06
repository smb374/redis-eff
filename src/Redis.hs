{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Redis (
    runRedis,
) where

import Control.Monad (forever, void, (>=>))
import qualified Control.Monad.Catch as E
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Function ((&))
import Data.UnixTime (UnixTime)
import Effectful
import Effectful.Concurrent (Concurrent, forkFinally, myThreadId, runConcurrent)
import Effectful.Concurrent.STM (TVar)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Provider
import qualified Effectful.Reader.Static as RS
import qualified Effectful.State.Static.Local as SL
import Effects
import Network.Socket (HostName, ServiceName, Socket, close, gracefulClose)
import Options
import System.Log.FastLogger (LogType' (LogStderr), ToLogStr (toLogStr), defaultBufSize)
import Text.Printf (printf)
import UnliftIO.STM (newTVarIO)
import Utils

type BackStore = AtomicKVStore ByteString (ByteString, Maybe UnixTime)

data RedisParams = RedisParams
    { redisHost :: Maybe HostName
    , redisPort :: ServiceName
    , redisRole :: NodeRole
    }
    deriving (Eq)

defaultRedisParams :: RedisParams
defaultRedisParams =
    RedisParams
        { redisHost = Nothing
        , redisPort = "6379"
        , redisRole = Master
        }

updateRedisParams :: Args -> RedisParams -> RedisParams
updateRedisParams a rs = rs{redisPort = rport, redisRole = rrole}
  where
    rrole = case replicaOf a of
        Just s -> case words s of
            [h, p] -> Slave (SlaveInfo{sport = p, shost = h})
            _ -> Master
        Nothing -> Master
    rport = maybe "6379" show (port a)

runRedis :: IO ()
runRedis = do
    args <- getArgs
    let ps = updateRedisParams args defaultRedisParams
    store <- newTVarIO mempty
    rstate <- newTVarIO $ emptyReplicationState{replRole = redisRole ps}
    (logger, clean) <- newTimedLevelLogger All (LogStderr defaultBufSize) True
    result <-
        redis ps rstate
            & (RS.runReader store . runAtomicKVStoreWithHashMap)
            & (runErrorNoCallStack . SL.evalState Nothing . runServer)
            & (runErrorNoCallStack . runProvider_ runDataIOWithSocket)
            & runLogging logger
            & runTimer
            & runConcurrent
            & runEff
    clean
    -- case result of
    --     Left (DataIOError e) -> hPutStrLn stderr $ printf "IO error on DataIO: %s" (show e)
    --     Right (Left serverError) -> case serverError of
    --         ServerNotInitialized act -> hPutStrLn stderr $ printf "Server not initialized for [%s]" act
    --         ServerIOError e -> hPutStrLn stderr $ printf "IO error on Server: %s" (show e)
    --     Right (Right _) -> return ()
    return ()

redis ::
    ( IOE :> es
    , BackStore :> es
    , Concurrent :> es
    , Logging :> es
    , Provider_ DataIO Socket :> es
    , Server :> es
    , Timer :> es
    ) =>
    RedisParams ->
    TVar ReplicationState ->
    Eff es ()
redis params rstate = do
    bindAddress (redisHost params) (redisPort params)
    logInfo $
        "Listening on "
            <> maybe "localhost" toLogStr (redisHost params)
            <> ":"
            <> toLogStr (redisPort params)
    case redisRole params of
        Master -> logInfo "Role = Master"
        Slave (SlaveInfo{..}) -> logInfo $ "Role = Slave -> " <> toLogStr shost <> ":" <> toLogStr sport
    forever $
        E.bracketOnError acceptClient (liftIO . close . fst) $
            \(sock, addr) -> do
                logInfo $ "Got connection from <" <> toLogStr (show addr) <> ">"
                void $ forkFinally (process sock) (const . liftIO $ gracefulClose sock 5000)
  where
    process sock = provideWith_ sock $ do
        result <-
            handler
                & (runErrorNoCallStack . runParsing)
        case result of
            Left err -> logError $ toLogStr (show err)
            Right _ -> return ()

handler ::
    ( BackStore :> es
    , Concurrent :> es
    , DataIO :> es
    , Logging :> es
    , Parsing :> es
    , Timer :> es
    ) =>
    Eff es ()
handler = myThreadId >>= loop
  where
    loop tid = do
        readBytes 1024 >>= \case
            Nothing -> logDebug $ toLogStr (printf "<%s>: Connection closed." (show tid) :: String)
            Just s -> do
                (_rest, cmd) <- parseCommand s
                logDebug $ toLogStr (printf "<%s>: Command [%s]" (show tid) (show cmd) :: String)
                handleCommand cmd
                loop tid

handleCommand :: (BackStore :> es, DataIO :> es, Timer :> es) => Command -> Eff es ()
handleCommand cmd = case cmd of
    PING -> writeBytes . encodeProtocol $ SimpleString "PONG"
    ECHO s -> writeBytes . encodeProtocol $ SimpleString s
    GET s ->
        ( lookupKV
            >=> maybe
                (pure Null)
                ( \(v, mexpr) -> do
                    ct <- getTime
                    case compare ct <$> mexpr of
                        Just LT -> return $ BulkString (C.length v) v
                        Nothing -> return $ BulkString (C.length v) v
                        _ -> deleteKV s >> return Null
                )
            >=> writeBytes . encodeProtocol
        )
            s
    SET k v mcond getOld mexpr -> do
        exprt <- maybe (pure Nothing) expire2unix mexpr
        oval <- lookupKV k
        let change = case (mcond, oval) of
                (Nothing, _) -> True
                (Just NX, Nothing) -> True
                (Just XX, Just _) -> True
                _ -> False
        case (change, oval, mexpr) of
            (True, Just (_, ttl), Just KEEPTTL) -> writeKV k (v, ttl) -- KEEPTTL write
            (True, _, _) -> writeKV k (v, exprt) -- SimpleWrite
            _ -> return ()
        writeBytes . encodeProtocol $ case fst <$> oval of
            Just ov -> if' getOld (BulkString (C.length ov) ov) $ if' change (SimpleString "OK") Null
            Nothing -> if' getOld Null $ if' change (SimpleString "OK") Null
    _ -> undefined
