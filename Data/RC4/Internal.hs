{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.RC4.Internal
    (
    ) where

-- | Usage:
-- Basically just the state monad.
-- 'replicateM n' gets n bytes, 'replicateM_ n discards n bytes, etc.

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.IO
import           Data.Array.ST
import           Data.Bits
import qualified Data.ByteString as B
import           Data.IORef
import           Data.STRef
import           Data.Word

class Monad m => Wat m where
    type Ref m :: * -> *
    type Arr m :: *
    newRef   :: forall a. a -> m ((Ref m) a)
    readRef  :: Ref m a -> m a
    writeRef :: Ref m a -> a -> m ()

instance Wat IO where
    type Ref IO = IORef
    type Arr IO = IOUArray Word8 Word8
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef

instance Wat (ST s) where
    type Ref (ST s) = STRef s
    type Arr (ST s) = STUArray s Word8 Word8
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef

data RC4 m = RC4 (Ref m Word8) (Ref m Word8) (Arr m)

-------------------------------------
-- Key scheduling algorithm
-------------------------------------

schedule :: forall m. Wat m => B.ByteString -> m (RC4 m)
schedule key = do
    (arr :: Arr m) <- newListArray (minBound :: Word8, maxBound) words -- :: m (Arr m)
    let mix :: Word8 -> Word8 -> m Word8
        mix j i = do
            ai <- readArray arr i
            let j' = j + ai + B.index key (fromIntegral i `mod` len)
            swap arr i j'
            return j'
    foldM_ mix 0 words
    (ir :: Ref m Word8) <- newRef 0
    (jr :: Ref m Word8) <- newRef 0
    return (RC4 ir jr (arr :: Arr m) :: RC4 m)

  where

    len = B.length key
    words = [minBound..maxBound]

-- -------------------------------------
-- -- Pseudo-random generation algorithm
-- -------------------------------------

-- generate :: RC4 -> IO Word8
-- generate (RC4 ir jr arr) = do
--     i <- readIORef ir
--     j <- readIORef jr
--     let i' = i + 1
--     j' <- (+) j <$> readArray arr i'
--     swap arr i' j'
--     writeIORef ir i
--     writeIORef jr j
--     (+) <$> readArray arr i' <*> readArray arr j' >>= readArray arr

-- -------------------------------------
-- -- CONVENIENCE
-- -------------------------------------

-- produce :: RC4 -> Int -> IO B.ByteString
-- produce rc4 = fmap B.pack . flip replicateM (generate rc4)

-- produce_ :: RC4 -> Int -> IO ()
-- produce_ rc4 = flip replicateM_ (generate rc4)

-- combine :: RC4 -> B.ByteString -> IO B.ByteString
-- combine rc4 b = zipWith' xor b <$> produce rc4 (B.length b)

-- -------------------------------------
-- -- HELPERS
-- -------------------------------------

swap :: (Ix i, MArray a e m) => a i e -> i -> i -> m ()
swap arr i j = do
    ai <- readArray arr i
    aj <- readArray arr j
    writeArray arr i aj
    writeArray arr j ai

-- -- Data.ByteString has a rewrite rule for zipWith to make this not stupidly inefficient
-- -- This is pointful because I don't know how clever the rewrite pragra is.
-- zipWith' f a b = B.pack $ B.zipWith f a b


