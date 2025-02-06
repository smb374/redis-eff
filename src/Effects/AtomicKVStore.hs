{-# LANGUAGE TemplateHaskell #-}

module Effects.AtomicKVStore (
    -- * Effect
    AtomicKVStore,

    -- ** Actions
    lookupKV,
    updateKV,
    writeKV,
    deleteKV,
    lookupOrThrowKV,
    existsKV,
    modifyKV,

    -- ** Handler
    runAtomicKVStoreWithHashMap,
)
where

import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Maybe (isJust)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TVar, atomically, modifyTVar, readTVar)
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic
import qualified Effectful.Reader.Static as RS
import Effectful.TH (makeEffect)

data AtomicKVStore k v :: Effect where
    LookupKV :: k -> AtomicKVStore k v m (Maybe v)
    UpdateKV :: k -> (Maybe v) -> AtomicKVStore k v m ()
type instance DispatchOf (AtomicKVStore k v) = Dynamic

makeEffect ''AtomicKVStore

writeKV :: AtomicKVStore k v :> es => k -> v -> Eff es ()
writeKV k = updateKV k . Just
{-# INLINE writeKV #-}

deleteKV :: AtomicKVStore k v :> es => k -> Eff es ()
deleteKV k = updateKV k Nothing
{-# INLINE deleteKV #-}

lookupOrThrowKV :: (AtomicKVStore k v :> es, Error e :> es) => (k -> e) -> k -> Eff es v
lookupOrThrowKV f k = do
    res <- lookupKV k
    case maybe (Left $ f k) Right res of
        Left e -> throwError e
        Right v -> return v

existsKV :: AtomicKVStore k v :> es => k -> Eff es Bool
existsKV = fmap isJust . lookupKV

modifyKV
    :: AtomicKVStore k v :> es
    => v
    -- ^ Default value if the key isn't present
    -> (v -> v)
    -> k
    -> Eff es ()
modifyKV d f k =
    lookupKV k >>= \case
        Just v -> writeKV k $ f v
        Nothing -> writeKV k $ f d

runAtomicKVStoreWithHashMap
    :: (Hashable k, RS.Reader (TVar (HashMap k v)) :> es, Concurrent :> es)
    => Eff (AtomicKVStore k v : es) a
    -> Eff es a
runAtomicKVStoreWithHashMap = interpret $ \_ -> \case
    LookupKV k -> do
        store <- RS.ask
        atomically $ do
            m <- readTVar store
            return $ m !? k
    UpdateKV k v -> do
        store <- RS.ask
        atomically $ modifyTVar store $ HM.alter (const v) k
