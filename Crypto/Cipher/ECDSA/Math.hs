module Crypto.Cipher.ECDSA.Math where


data Point = Point Integer Integer
           | PointO
             deriving (Show, Eq)

-- |2^2 = x^3 + a*x + b mod P
data Curve = Curve 
  Integer -- ^a
  Integer -- ^b
  Integer -- ^P
  deriving (Show)

type PrivateKey = Integer
type PublicKey = Point

-- |Calculate a / b (mod c)
modDiv :: Integer -> Integer -> Integer -> Maybe Integer
modDiv a b p = do
  mibp <- modInv b p
  return ((a * mibp) `mod` p)

modInv :: Integer -> Integer -> Maybe Integer
modInv a m
  | x == 1 = return (y `mod` m)
  | otherwise = Nothing
    where
      (x, y, _) = egcd a m

      egcd :: Integer -> Integer -> (Integer, Integer, Integer)
      egcd a b
        | b == 0 = (a, 1, 0)
        | otherwise = (d, t, s - (a `div` b) * t)
          where
            (d, s, t) = egcd b (a `mod` b)

-- |Elliptic curve point addition
pointAdd :: Curve -> Point -> Point -> Maybe Point
pointAdd c p q
  | p == PointO = Just q
  | q == PointO = Just p
  | p == Point xq (-yq) = Just PointO
  | p == q = pointDouble c p 
  | otherwise = do
    l <- modDiv (yp - yq) (xp - xq) pr
    let xr = (l ^ 2 - xp - xq) `mod` pr
    let yr = (l * (xp - xr) - yp) `mod` pr
    return (Point xr yr)
  where
    (Curve _ _ pr) = c
    (Point xp yp) = p
    (Point xq yq) = q

pointDouble :: Curve -> Point -> Maybe Point
pointDouble c p
  | p == PointO = Just PointO
  | otherwise = do
    l <- modDiv (3 * xp ^ 2 + a) (2 * yp) pr
    let xr = (l ^ 2 - 2 * xp) `mod` pr
    let yr = (l * (xp - xr) - yp) `mod` pr
    return (Point xr yr)
  where
    (Curve a _ pr) = c
    (Point xp yp) = p

pointMul :: Curve -> Integer -> Point -> Maybe Point
pointMul c n p 
  | n == 0 = return PointO
  | n `mod` 2 == 1 = do
      p' <- pointMul c (n - 1) p
      pointAdd c p p'
  | otherwise = do
      p' <- pointDouble c p
      pointMul c (n `div` 2) p'
   
-- |Check that point satisfies elliptic curve equation
onCurve :: Curve -> Point -> Bool
-- onCurve _ Point0 = True    TODO: Is this true?
onCurve (Curve a b p) (Point x y) = 
  (y ^ 2) `mod` p == (x ^ 3 + a * x + b) `mod` p

