module Options (Args (..), getArgs) where

import Options.Applicative

data Args = Args
    { port :: Maybe Int
    , replicaOf :: Maybe String
    }
    deriving (Show, Eq)

getArgs :: IO Args
getArgs = execParser $ info (helper <*> args) (fullDesc <> header "redis-eff: A small redis clone in Haskell & Effectful")

args :: Parser Args
args =
    Args
        <$> optional
            ( option
                auto
                ( long "port"
                    <> short 'p'
                    <> help "Port to bind Redis"
                    <> metavar "PORT"
                )
            )
        <*> optional
            ( strOption
                ( long "replicaof"
                    <> short 'r'
                    <> help "Host to replicate"
                    <> metavar "\"MASTER_HOST MASTER_PORT\""
                )
            )
