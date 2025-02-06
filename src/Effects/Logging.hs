{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Effects.Logging (
    -- * Effect
    Logging,

    -- ** Data
    Level (..),
    TimedLevelLogger (..),

    -- ** Initializer
    newTimedLevelLogger,

    -- ** Actions
    logError,
    logWarn,
    logInfo,
    logDebug,

    -- ** Handler
    runLogging,
) where

import Control.Monad (when)
import Effectful
import Effectful.Dispatch.Static
import System.Console.ANSI
import System.Log.FastLogger

data Level = Error | Warn | Info | Debug | All deriving (Show, Eq)

instance Ord Level where
    compare Error l = case l of
        Error -> EQ
        _ -> GT
    compare Warn l = case l of
        Error -> LT
        Warn -> EQ
        _ -> GT
    compare Info l = case l of
        All -> GT
        Debug -> GT
        Info -> EQ
        _ -> LT
    compare Debug l = case l of
        All -> GT
        Debug -> EQ
        _ -> LT
    compare All l = case l of
        All -> EQ
        _ -> LT

data TimedLevelLogger
    = TimedLevelLogger
    { logF :: TimedFastLogger
    , filterLevel :: Level
    , colored :: Bool
    }

newTimedLevelLogger :: Level -> LogType -> Bool -> IO (TimedLevelLogger, IO ())
newTimedLevelLogger lvl typ isc = do
    tsrc <- newTimeCache timefmt
    (logger, clean) <- newTimedFastLogger tsrc typ
    return (TimedLevelLogger{logF = logger, filterLevel = lvl, colored = isc}, clean)

-- ANSI codes
timefmt :: TimeFormat
timefmt = "%Y-%m-%dT%H%M%S%z"

resetCode :: String
resetCode = setSGRCode [Reset]

formatLevel :: Level -> LogStr
formatLevel Error =
    toLogStr (setSGRCode [SetColor Foreground Dull Red, SetConsoleIntensity BoldIntensity])
        <> "ERROR"
        <> toLogStr resetCode
formatLevel Warn =
    toLogStr (setSGRCode [SetColor Foreground Dull Yellow, SetConsoleIntensity BoldIntensity])
        <> "WARN "
        <> toLogStr resetCode
formatLevel Info =
    toLogStr (setSGRCode [SetColor Foreground Dull Green, SetConsoleIntensity BoldIntensity])
        <> "INFO "
        <> toLogStr resetCode
formatLevel Debug =
    toLogStr (setSGRCode [SetColor Foreground Dull Blue, SetConsoleIntensity BoldIntensity])
        <> "DEBUG"
        <> toLogStr resetCode
formatLevel All =
    toLogStr (setSGRCode [SetColor Foreground Vivid Black, SetConsoleIntensity BoldIntensity])
        <> "...  "
        <> toLogStr resetCode

formatLevelPlain :: Level -> LogStr
formatLevelPlain Error = "[ ERROR ]"
formatLevelPlain Warn = "[ WARN  ]"
formatLevelPlain Info = "[ INFO  ]"
formatLevelPlain Debug = "[ DEBUG ]"
formatLevelPlain All = "[ ...   ]"

logWithTime, logWithTime_ :: Level -> LogStr -> (FormattedTime -> LogStr)
logWithTime lvl s time =
    toLogStr (setSGRCode [SetColor Foreground Dull White])
        <> toLogStr time
        <> toLogStr resetCode
        <> " "
        <> formatLevel lvl
        <> " "
        <> s
        <> "\n"
logWithTime_ lvl s time =
    toLogStr time
        <> " "
        <> formatLevelPlain lvl
        <> " "
        <> s
        <> "\n"

data Logging :: Effect
type instance DispatchOf Logging = Static WithSideEffects
newtype instance StaticRep Logging = Logging TimedLevelLogger

logL :: Logging :> es => Level -> LogStr -> Eff es ()
logL lvl msg = do
    Logging TimedLevelLogger{..} <- getStaticRep
    when (lvl > filterLevel) . unsafeEff_ $
        if colored
            then logF (logWithTime lvl msg)
            else logF (logWithTime_ lvl msg)

logError, logWarn, logInfo, logDebug :: Logging :> es => LogStr -> Eff es ()
logError = logL Error
logWarn = logL Warn
logInfo = logL Info
logDebug = logL Debug

runLogging :: IOE :> es => TimedLevelLogger -> Eff (Logging : es) a -> Eff es a
runLogging logger = evalStaticRep (Logging logger)
