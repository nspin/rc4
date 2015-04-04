module Data.RC4
    ( RC4
    , ksa
    , prga
    , encrypt
    ) where

-- | Usage:
-- Basically just the state monad.
-- 'replicateM n' gets n bytes, 'replicateM_ n discards n bytes, etc.

import           Data.Array.Unboxed
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Word
import           Control.Monad.State.Lazy

type Arr = UArray Word8 Word8

data RC4 = RC4 Word8 Word8 Arr

-------------------------------------
-- Key scheduling algorithm
-------------------------------------

ksa :: B.ByteString -> RC4
ksa key = RC4 0 0 . snd $ foldl mix (0, start) words

  where

    len = B.length key
    words = [minBound..maxBound]
    start = listArray (minBound, maxBound) words

    mix :: (Word8, Arr) -> Word8 -> (Word8, Arr)
    mix (j, arr) i = let j' = j + arr!i + B.index key (fromIntegral i `mod` len)
                     in (j', swap i j' arr)

-------------------------------------
-- Pseudo-random generation algorithm
-------------------------------------

prga :: State RC4 Word8
prga = state $ \(RC4 i j arr) ->
    let i' = i + 1
        j' = j + arr!i'
        arr' = swap i j arr
    in (arr'!(arr'!i' + arr'!j'), RC4 i' j' arr')

-------------------------------------
-- CONVENIENCE
-------------------------------------

encrypt :: B.ByteString -> State RC4 B.ByteString
encrypt b = fmap (zipWith' xor b . B.pack) $ replicateM (B.length b) prga

-------------------------------------
-- HELPERS
-------------------------------------

swap :: (Ix i, IArray a e) => i -> i -> a i e -> a i e
swap x y arr = arr // [(x, arr!y), (y, arr!x)]

-- Data.ByteString has a rewrite rule for zipWith to make this not stupidly inefficient
-- This is pointful because I don't know how clever the rewrite pragra is.
zipWith' f a b = B.pack $ B.zipWith f a b

