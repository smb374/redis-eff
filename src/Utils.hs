module Utils (if', ms2us, s2us, randomBytes, hexEncode) where

import Control.Arrow (Arrow (second))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import System.Random (RandomGen (genWord8), StdGen)

if' :: forall a. Bool -> a -> a -> a
if' cond x y = if cond then x else y
{-# INLINE if' #-}

ms2us :: Integer -> Integer
ms2us = (* 1000)

s2us :: Integer -> Integer
s2us = (* 1000000)

randomBytes :: Int -> StdGen -> ByteString
randomBytes count = B.pack . step count
  where
    step 0 _ = []
    step c g = uncurry (:) $ second (step (c - 1)) $ genWord8 g

hexEncode :: ByteString -> ByteString
hexEncode = toStrict . toLazyByteString . byteStringHex
