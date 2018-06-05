{-|
Module : V2

2D Vector
-}
module V2 where

data V2 = V2 !Double !Double deriving Show

instance Num V2 where
    (V2 x1 y1) + (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
    (V2 x1 y1) * (V2 x2 y2) = V2 (x1 * x2) (y1 * y2)
    negate (V2 x1 y1) = V2 (negate x1) (negate y1)
    abs (V2 x1 y1) = V2 (abs x1) (abs y1)
    signum (V2 x1 y1) = V2 (signum x1) (signum y1)
    fromInteger x = V2 (fromIntegral x) (fromIntegral x)

instance Fractional V2 where
    fromRational x = V2 (fromRational x) (fromRational x)
    recip (V2 x y) = V2 (recip x) (recip y)
