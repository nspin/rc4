{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Crypto.Cipher.RC4

import           Control.Monad.ST
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List
import           Data.Word
import           System.Exit

-- TODO: Be a bit more rigorous
main :: IO ()
main = do
    ok <- stToIO $ check exp "key" "hello" 0
    if ok then exitSuccess else exitFailure
 where exp = B.pack [0x63, 0x09, 0x58, 0x81, 0x4b]

-- Given a key and some data, prints the
encrypt :: B.ByteString -> B.ByteString -> Int -> ST s B.ByteString
encrypt key txt off = do
    r <- schedule key
    discard off r
    combine txt r

-- Yay functors
check :: B.ByteString -> B.ByteString -> B.ByteString -> Int -> ST s Bool
check expected = (fmap.fmap.fmap.fmap) (== expected) encrypt

toHex :: B.ByteString -> String
toHex = concatMap hex . B.unpack

hex :: Word8 -> String
hex w = map ((genericIndex "0123456789ABCDEF") . (`shiftR` 4)) [ w, w `shiftL` 4 ]

