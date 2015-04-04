module Test () where

import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List
import           Data.RC4
import           Data.Word
import           Control.Monad.State.Lazy
import           System.Environment

-- For use with the examples on /wiki/RC4

test :: B.ByteString -> B.ByteString -> (String, String)
test key txt = (toHex a, toHex b)
  where
    rc4 = ksa key
    a = B.pack $ evalState (replicateM (B.length txt) prga) rc4
    b = evalState (encrypt txt) rc4

toHex :: B.ByteString -> String
toHex = concatMap hex . B.unpack

hex :: Word8 -> String
hex w = map ((genericIndex "0123456789ABCDEF") . (`shiftR` 4)) [ w, w `shiftL` 4 ]

