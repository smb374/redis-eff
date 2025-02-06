{-# LANGUAGE TemplateHaskell #-}

module Effects.DataIO (
    -- * Effect
    DataIO,

    -- ** Data
    DataIOError (..),

    -- ** Actions
    readBytes,
    writeBytes,

    -- ** Handler
    runDataIOWithSocket,
) where

import Control.Monad.Catch (catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.TH (makeEffect)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as NSB

newtype DataIOError = DataIOError IOError deriving (Show, Eq)

data DataIO :: Effect where
    ReadBytes :: Int -> DataIO m (Maybe ByteString)
    WriteBytes :: ByteString -> DataIO m ()
type instance DispatchOf DataIO = Dynamic

makeEffect ''DataIO

runDataIOWithSocket :: (IOE :> es, Error DataIOError :> es) => Socket -> Eff (DataIO : es) a -> Eff es a
runDataIOWithSocket sock = interpret $ \_ -> \case
    ReadBytes n -> do
        res <- adapt $ NSB.recv sock n
        if B.null res
            then return Nothing
            else return $ Just res
    WriteBytes bs -> adapt $ NSB.sendAll sock bs
  where
    adapt m = liftIO m `catch` \(e :: IOError) -> throwError $ DataIOError e
