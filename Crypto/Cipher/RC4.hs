module Crypto.Cipher.RC4
    ( RC4
    , schedule
    , generate
    , produce
    , produce_
    , combine
    ) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Bits
import qualified Data.ByteString as B
import           Data.STRef
import           Data.Word

data RC4 s = RC4 (STRef s Word8) (STRef s Word8) (STUArray s Word8 Word8)

-------------------------------------
-- Key scheduling algorithm
-------------------------------------

schedule :: B.ByteString -> ST s (RC4 s)
schedule key = do
    arr <- newListArray (minBound, maxBound) words
    let mix j i = do
            ai <- readArray arr i
            let j' = j + ai + B.index key (fromIntegral i `mod` len)
            swap arr i j'
            return j'
    foldM_ mix 0 words
    ir <- newSTRef 0
    jr <- newSTRef 0
    return $ RC4 ir jr arr

  where

    len = B.length key
    words = [minBound..maxBound]

-------------------------------------
-- Pseudo-random generation algorithm
-------------------------------------

generate :: RC4 s -> ST s Word8
generate (RC4 ir jr arr) = do
    i <- readSTRef ir
    j <- readSTRef jr
    let i' = i + 1
    j' <- (+) j <$> readArray arr i'
    swap arr i' j'
    writeSTRef ir i'
    writeSTRef jr j'
    (+) <$> readArray arr i' <*> readArray arr j' >>= readArray arr

-------------------------------------
-- CONVENIENCE
-------------------------------------

produce :: RC4 s -> Int -> ST s B.ByteString
produce r = fmap B.pack . flip replicateM (generate r)

produce_ :: RC4 s -> Int -> ST s ()
produce_ r = flip replicateM_ (generate r)

combine :: RC4 s -> B.ByteString -> ST s B.ByteString
combine r b = zipWith' xor b <$> produce r (B.length b)

-------------------------------------
-- HELPERS
-------------------------------------

swap :: (Ix i, MArray a e m) => a i e -> i -> i -> m ()
swap arr i j = do
    ai <- readArray arr i
    aj <- readArray arr j
    writeArray arr i aj
    writeArray arr j ai

-- Data.ByteString has a rewrite rule for zipWith to make this not stupidly inefficient
-- This is pointful because I don't know how clever the rewrite pragra is.
zipWith' f a b = B.pack $ B.zipWith f a b

