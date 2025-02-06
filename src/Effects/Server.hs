{-# LANGUAGE TemplateHaskell #-}

module Effects.Server (
    -- * Effect
    Server,

    -- ** Data
    ServerError (..),

    -- ** Actions
    bindAddress,
    acceptClient,
    closeServer,

    -- ** Handler
    runServer,
) where

import Control.Monad.Catch (catch)
import qualified Control.Monad.Catch as E
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import qualified Effectful.State.Static.Local as SL
import Effectful.TH (makeEffect)
import Network.Socket
import Text.Printf (printf)

data ServerError
    = ServerNotInitialized String
    | ServerIOError IOError
    deriving (Eq)

instance Show ServerError where
    show (ServerNotInitialized act) = printf "Sever hasn't been initialized for [%s]" act
    show (ServerIOError e) = show e

data Server :: Effect where
    BindAddress :: Maybe HostName -> ServiceName -> Server m ()
    AcceptClient :: Server m (Socket, SockAddr)
    CloseServer :: Server m ()
type instance DispatchOf Server = Dynamic

makeEffect ''Server

runServer
    :: (IOE :> es, Error ServerError :> es, SL.State (Maybe Socket) :> es)
    => Eff (Server : es) a
    -> Eff es a
runServer = interpret $ \_ -> \case
    BindAddress host port -> do
        sock <- adapt (open =<< resolve host port)
        SL.put (Just sock)
    AcceptClient ->
        SL.get >>= \case
            Nothing -> throwError $ ServerNotInitialized "acceptClient"
            Just sock -> adapt $ accept sock
    CloseServer ->
        SL.get >>= \case
            Nothing -> return ()
            Just sock -> adapt $ close sock
  where
    adapt m = liftIO m `catch` \(e :: IOError) -> throwError $ ServerIOError e
    resolve host port = do
        let hints =
                defaultHints
                    { addrFlags = [AI_PASSIVE]
                    , addrSocketType = Stream
                    }
        head <$> getAddrInfo (Just hints) host (Just port)

    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
