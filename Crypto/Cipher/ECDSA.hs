module Crypto.Cipher.ECDSA where

import Crypto.Cipher.ECDSA.Util
import Crypto.Cipher.ECDSA.Math

import qualified Data.ByteString.Lazy as BS
import Data.Word (Word8)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Read (hexadecimal)
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.Random (randomRIO)
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import Control.Monad.State

signz :: CurveParameters
      -> PrivateKey
      -> Integer
      -> IO (Integer, Integer)
signz cp da z = do
  k <- randomRIO (1, n - 1)
  maybe (signz cp da z) return (getRS k)  
  where
    (CurveParameters _ c@(Curve a b fr) g n) = cp
    getRS k = do
      kg <- pointMul c k g
      r <- case kg of
        Point x1 y1 | x1 `mod` n /= 0 -> Just (x1 `mod` n)
        _ -> Nothing
      k1 <- modInv k n
      case k1 * (z + r * da) `mod` n of
        0 -> Nothing
        s -> return (r, s)

verifyz :: CurveParameters
        -> PublicKey
        -> (Integer, Integer)
        -> Integer
        -> Bool
verifyz cp qa (r, s) z
  | not (valid r && valid s) = False
  | otherwise = fromMaybe False go
  where
    (CurveParameters _ c@(Curve a b fr) g n) = cp

    valid x = x > 0 && x < n

    go = do
      w <- modInv s n
      let (u1, u2) = ((z * w) `mod` n, (r * w) `mod` n)
      u1g <- pointMul c u1 g
      u2qa <- pointMul c u2 qa
      xy <- pointAdd c u1g u2qa
      case xy of
        Point x1 y1 -> return (x1 == r `mod` n)
        _ -> Nothing

data CurveParameters = CurveParameters
  Integer -- Size
  Curve   -- 
  Point   -- ^G  - Base point
  Integer -- ^n  - The order of G
  deriving (Show)

type HashingFunction = BS.ByteString -> BS.ByteString

-- |n must have a bitsize multiple of 8
sign :: CurveParameters 
     -> HashingFunction 
     -> PrivateKey 
     -> BS.ByteString 
     -> IO BS.ByteString
sign cp hf da m = do
  let e = hf m
  let z = bytesToInteger (BS.take (fromIntegral ln) e)
  (r, s) <- signz cp da z
  return (BS.concat (map (padTo (ln `div` 8) . integerToBytes) [r, s]))
  where
    (CurveParameters ln _ _ n) = cp

verify :: CurveParameters
       -> HashingFunction
       -> PublicKey
       -> BS.ByteString -- sig
       -> BS.ByteString
       -> Bool
verify cp hf qa sig m = verifyz cp qa (r, s) z
  where
    e = hf m
    z = bytesToInteger (BS.take (fromIntegral ln) e)
    (r, s) = (
      bytesToInteger $ BS.take (fromIntegral $ ln `div` 8) sig,
      bytesToInteger $ BS.drop (fromIntegral $ ln `div` 8) sig)      
    (CurveParameters ln _ _ n) = cp


