module LinAlg where

data V2 = V2 { x :: Float, y :: Float}

instance Num V2 where
    (+) a b = V2 (x a + x b) (y a + y b)
    (*) a b = V2 (x a * x b) (y a * y b)
    (-) a b = V2 (x a - x b) (y a - y b)
    abs a    = V2 (abs $ x a) (abs $ y a)
    signum a = V2 (signum $ x a) (signum $ y a)
    fromInteger a = V2 (fromInteger a) (fromInteger a)

dotProd :: V2 -> V2 -> Float
dotProd a b = x a * x b + y a * y b

dotSq :: V2 -> Float
dotSq a = dotProd a a
