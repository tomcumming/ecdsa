module Crypto.Cipher.ECDSA.Util where

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Read (hexadecimal)
import qualified Data.ByteString.Lazy as BS

readHexInteger :: Text -> Integer
readHexInteger = either error fst . hexadecimal

-- |Calulate the length in bits of an Integer (log2)
bitSize :: Integer -> Integer
bitSize n = case n of
  0 -> 0
  n -> 1 + bitSize (n `div` 2)

-- |I guess this is too slow and needs to be replaced
bytesToInteger :: BS.ByteString -> Integer
bytesToInteger = BS.foldl' (\n c -> n * 256 + fromIntegral c) 0

integerToBytes :: Integer -> BS.ByteString
integerToBytes = BS.pack . go
  where
    go c = case c of
      0 -> [] 
      c -> go (c `div` 256) ++ [fromIntegral (c `mod` 256)]

padTo :: Integer -> BS.ByteString -> BS.ByteString
padTo n s = BS.append padding s
  where
    padding = BS.pack 
      (replicate (fromIntegral n - fromIntegral (BS.length s)) 0)

