module Test () where

import           Data.RC4.IO

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List
import           Data.Word
import           Control.Monad.State.Lazy
import           System.Environment

-- For use with the examples on /wiki/RC4

-- test :: B.ByteString -> B.ByteString -> (String, String)
-- test key txt = (toHex a, toHex b)
--   where
--     r = schedule key
--     a = evalState (produce (B.length txt)) r
--     b = evalState (combine txt) r

test :: B.ByteString -> B.ByteString -> IO ()
test key txt = do
    rc4 <- schedule key
    enc <- combine rc4 txt
    putStrLn $ toHex enc

toHex :: B.ByteString -> String
toHex = concatMap hex . B.unpack

hex :: Word8 -> String
hex w = map ((genericIndex "0123456789ABCDEF") . (`shiftR` 4)) [ w, w `shiftL` 4 ]

