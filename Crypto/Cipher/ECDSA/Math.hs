module Crypto.Cipher.ECDSA.Math where

data Point = Point Integer Integer
           | Point0
             deriving (Show, Eq)

-- | 2^2 = x^3 + a*x + b mod P
data Curve = Curve 
  { ca :: Integer
  , cb :: Integer
  , cp :: Integer
  } -- deriving Show
    -- Do we really need Show or was this just for development?

type PrivateKey = Integer
type PublicKey  = Point

-- | Calculate a / b (mod c)
modDiv :: Curve -> Maybe Integer
modDiv (Curve a b p) = do
  mibp <- modInv b p
  return ((a * mibp) `mod` p)

modInv :: Integer -> Integer -> Maybe Integer
modInv a m
  | x == 1 = return (y `mod` m)
  | otherwise = Nothing
    where
      (x, y, _) = egcd a m

      -- Re-write this and the above maybe?
      egcd :: Integer -> Integer -> (Integer, Integer, Integer)
      egcd a' b
        | b ==  0 = (a', 1, 0)
        | otherwise = (d, t, s - (a' `div` b) * t)
          where
            (d, s, t) = egcd b (a' `mod` b)

-- | Elliptic curve point addition
pointAdd :: Curve -> Point -> Point -> Maybe Point
pointAdd _ _ Point0 = Nothing -- Confirm?
pointAdd _ Point0 _ = Nothing -- Confirm?
pointAdd c@(Curve _ _ pr) p@(Point xp yp) q@(Point xq yq)
  | p == Point0 = Just q
  | q == Point0 = Just p
  | p == Point xq (-yq) = Just Point0
  | p == q = pointDouble c p 
  | otherwise = do
    l <- modDiv $ Curve (yp - yq) (xp - xq) pr
    let xr = (l ^ 2   - xp  - xq) `mod` pr
        yr = (l * (xp - xr) - yp) `mod` pr
    return (Point xr yr)

pointDouble :: Curve -> Point -> Maybe Point
pointDouble _ Point0 = Nothing  -- Confirm?
pointDouble (Curve a1 _ pr) p@(Point xp yp)
  | p == Point0 = Just Point0
  | otherwise = do
    l <- modDiv $ Curve (3 * xp ^ 2 + a1) (2 * yp) pr
    let xr = (l ^ 2 - 2 * xp) `mod` pr
        yr = (l * (xp - xr) - yp) `mod` pr
    return (Point xr yr)

pointMul :: Curve -> Integer -> Point -> Maybe Point
pointMul c n p 
  | n == 0 = return Point0
  | n `mod` 2 == 1 = do
      p' <- pointMul c (n - 1) p
      pointAdd  c p p'
  | otherwise = do
      p' <- pointDouble c p
      pointMul c (n `div` 2) p'
   
-- | Check that point satisfies elliptic curve equation
onCurve :: Curve -> Point -> Bool
onCurve _ Point0 = True
onCurve (Curve a b p) (Point x y) = 
  (y ^ 2) `mod` p == (x ^ 3 + a * x + b) `mod` p
