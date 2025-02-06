{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Replication (
    -- * Effect
    Replication,

    -- ** Data
    NodeRole (..),
    SlaveInfo (..),
    ReplicationInfo (..),
    ReplicationState (..),

    -- ** Actions
    emptyReplicationState,
    formatReplicationInfo,
    initReplication,
    getReplicationInfo,
    handShake,

    -- ** Handler
    runReplication,
) where

import Control.Monad (void)
import qualified Control.Monad.Catch as E
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TVar, atomically, modifyTVar, readTVar)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Provider (provideWith_, runProvider_)
import qualified Effectful.State.Static.Shared as SS
import Effectful.TH (makeEffect)
import Effects.DataIO
import Effects.Logging
import Effects.Parsing
import Network.Socket
import System.Random (getStdGen)
import Text.Printf (printf)
import Utils

data NodeRole
    = Master
    | Slave SlaveInfo
    deriving (Eq)

instance Show NodeRole where
    show Master = "role:master"
    show Slave{} = "role:slave"

data SlaveInfo = SlaveInfo
    { shost :: HostName
    , sport :: ServiceName
    }
    deriving (Eq)

data ReplicationInfo = ReplicationInfo
    { replID :: ByteString
    , replOffset :: Int
    , isSlave :: Bool
    }
    deriving (Show, Eq)

data ReplicationState = ReplicationState
    { replRole :: NodeRole
    , replInfo :: ReplicationInfo
    , replClient :: Maybe Socket
    }
    deriving (Show, Eq)

emptyReplicationState :: ReplicationState
emptyReplicationState =
    ReplicationState
        { replRole = Master
        , replInfo = ReplicationInfo{replID = C.empty, replOffset = 0, isSlave = False}
        , replClient = Nothing
        }

data Replication :: Effect where
    InitReplication :: Replication m ()
    GetReplicationInfo :: Replication m ReplicationInfo
    HandShake :: Replication m ()

type instance DispatchOf Replication = Dynamic

makeEffect ''Replication

runReplication
    :: ( IOE :> es
       , SS.State (TVar ReplicationState) :> es
       , Concurrent :> es
       , Parsing :> es
       , Logging :> es
       )
    => Eff (Replication : es) a
    -> Eff es a
runReplication = interpret $ \_ -> \case
    InitReplication -> do
        rState <- SS.get
        role <- atomically $ replRole <$> readTVar rState
        case role of
            Master -> do
                rID <- hexEncode . randomBytes 20 <$> liftIO getStdGen
                atomically $ modifyTVar rState $ \rs -> rs{replInfo = (replInfo rs){replID = rID}}
            Slave (SlaveInfo{..}) -> do
                sock <- liftIO (open =<< resolve shost sport)
                atomically $ modifyTVar rState $ \rs ->
                    rs{replRole = role, replInfo = (replInfo rs){isSlave = True}, replClient = Just sock}
    GetReplicationInfo -> do
        rState <- SS.get
        atomically $ replInfo <$> readTVar rState
    HandShake -> do
        rState <- SS.get
        sock <- atomically $ do
            msock <- replClient <$> readTVar rState
            maybe undefined return msock
        _ <- runErrorNoCallStack . runProvider_ runDataIOWithSocket . runErrorNoCallStack $ provideWith_ sock handShakeStep
        return ()
  where
    resolve host port = do
        let hints = defaultHints{addrSocketType = Stream}
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

handShakeStep :: (DataIO :> es, Parsing :> es, Error ParseError :> es, Logging :> es) => Eff es ()
handShakeStep = do
    void $ sendCommand PING ((== SimpleString "PONG"), "PONG")
    logDebug "Stage 1: PING complete"
    return ()

sendCommand :: (DataIO :> es, Parsing :> es, Error ParseError :> es) => Command -> (ProtocolData -> Bool, String) -> Eff es ProtocolData
sendCommand cmd (verify, expResp) = do
    writeBytes $ encodeCommand cmd
    readBytes 1024 >>= \case
        Nothing -> throwError EmptyInput
        Just s -> do
            (_, proto) <- parseProtocol s
            if' (verify proto) (return proto) (throwError $ ValidationFailed expResp (show proto))

formatReplicationInfo :: ReplicationInfo -> ByteString
formatReplicationInfo ReplicationInfo{..} =
    encodeProtocol $
        ArrayType 4 $
            V.fromList
                [ BulkString (C.length header) header
                , BulkString (C.length role) role
                , BulkString (C.length rid) rid
                , BulkString (C.length offset) offset
                ]
  where
    header = "# Replication"
    role = if' isSlave "role:slave" "role:master"
    rid = C.append "master_replid:" replID
    offset = C.pack $ printf "master_repl_offset:%d" replOffset
