module Effects.Timer (
    -- * Effect
    Timer,

    -- ** Action
    getTime,

    -- * Handler
    runTimer,
)
where

import Data.UnixTime (UnixTime, getUnixTime)
import Effectful
import Effectful.Dispatch.Static

data Timer :: Effect
type instance DispatchOf Timer = Static WithSideEffects
data instance StaticRep Timer = Timer

getTime :: Timer :> es => Eff es UnixTime
getTime = do
    Timer <- getStaticRep
    unsafeEff_ getUnixTime

runTimer :: IOE :> es => Eff (Timer : es) a -> Eff es a
runTimer = evalStaticRep Timer
