module Data.RC4.IO
    ( RC4
    , schedule
    , generate
    , produce
    , produce_
    , combine
    ) where

-- | Usage:
-- Basically just the state monad.
-- 'replicateM n' gets n bytes, 'replicateM_ n discards n bytes, etc.

import           Control.Monad
import           Data.Array.IO
import           Data.Bits
import qualified Data.ByteString as B
import           Data.IORef
import           Data.Word

data RC4 = RC4 (IORef Word8) (IORef Word8) (IOUArray Word8 Word8)

-------------------------------------
-- Key scheduling algorithm
-------------------------------------

schedule :: B.ByteString -> IO RC4
schedule key = do
    arr <- newListArray (minBound, maxBound) words
    let mix :: Word8 -> Word8 -> IO Word8
        mix j i = do
            j' <- (+) (j + B.index key (fromIntegral i `mod` len)) <$> readArray arr i
            swap arr i j'
            return j'
    foldM_ mix 0 words
    ir <- newIORef 0
    jr <- newIORef 0
    return $ RC4 ir jr arr

  where

    len = B.length key
    words = [minBound..maxBound]

-------------------------------------
-- Pseudo-random generation algorithm
-------------------------------------

generate :: RC4 -> IO Word8
generate (RC4 ir jr arr) = do
    i <- readIORef ir
    j <- readIORef jr
    let i' = i + 1
    j' <- (+) j <$> readArray arr i'
    swap arr i' j'
    writeIORef ir i
    writeIORef jr j
    (+) <$> readArray arr i' <*> readArray arr j' >>= readArray arr

-------------------------------------
-- CONVENIENCE
-------------------------------------

produce :: RC4 -> Int -> IO B.ByteString
produce rc4 = fmap B.pack . flip replicateM (generate rc4)

produce_ :: RC4 -> Int -> IO ()
produce_ rc4 = flip replicateM_ (generate rc4)

combine :: RC4 -> B.ByteString -> IO B.ByteString
combine rc4 b = zipWith' xor b <$> produce rc4 (B.length b)

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

