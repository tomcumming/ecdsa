{-# LANGUAGE OverloadedStrings #-}

module Crypto.Cipher.ECDSA.Test where

import Crypto.Cipher.ECDSA
import Crypto.Cipher.ECDSA.Util
import Crypto.Cipher.ECDSA.Math

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as BS
import Crypto.Hash.SHA256 (hashlazy)

sha_it :: BS.ByteString -> BS.ByteString
sha_it = BS.fromChunks . return . hashlazy

main = do

  -- Curve-ID: brainpoolP160r1
  -- Curve parameters
  let c = Curve 0x340E7BE2A280EB74E2BE61BADA745D97E8F7C300
                0x1E589A8595423412134FAA2DBDEC95C8D8675E58
                0xE95E4A5F737059DC60DFC7AD95B3D8139515620F
  let g = Point 0xBED5AF16EA3F6A4F62938C4631EB5AF7BDBCDBC3
                0x1667CB477A1A8EC338F94741669C976316DA6321
  let n = 0xE95E4A5F737059DC60DF5991D45029409E60FC09
  let h = 1

  let cp = CurveParameters 160 c g n 
    

  -- Public and private keys
  let da = 916338557271912060709748568644112591278466742281
  let qa = Point 1185204462950103058838903871952479866892949799209 
                 205560904785823604751039452164442264739289219256

  let m  = encodeUtf8 ("Hello World!" :: Text)
  let m' = encodeUtf8 ("Hello World." :: Text)

  sig <- sign cp sha_it da m
  print $ verify cp sha_it qa sig m
  print $ verify cp sha_it qa sig m'


