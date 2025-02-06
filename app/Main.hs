module Main (main) where

import Redis (runRedis)

main :: IO ()
main = runRedis
