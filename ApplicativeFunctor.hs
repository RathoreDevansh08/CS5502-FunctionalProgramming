-- class (Functor f) => Applicative f where   
--    pure :: a -> f a   
--    (<*>) :: f (a -> b) -> f a -> f b

-- data Maybe a = Just a | Nothing

import Control.Applicative 

f1:: Int -> Int -> Int 
f1 x y = 2*x+y  
main = do  
   print(show $ f1 <$> (Just 1) <*> (Just 2) ) 
-- results in "Just 4"

-- let f = (*) <$> (Just 4)
-- now, f :: Num a => Maybe (a -> a)

-- <*> particularly for Maybe type
funcMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
funcMaybe Nothing _ = Nothing
funcMaybe (Just f) val = f <$> val

-- pure (4 *) <*> (Just 5)   same as   (pure (4 *)) <*> (Just 5)
-- output: Just 20

-- pure (*) <*> (Just 4) <*> (Just 5)
-- output: Just 20

-- pure (*) <*> Nothing <*> (Just 5)
-- Nothing

-- instance applicative for Maybe type
instance Applicative Maybe where
  pure = Just
  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just x) = Just (f x)

-- applicative instance for lists
instance Applicative [] where
  pure a = [a]
  fs <*> xs = [f x | f <- fs, x <- xs]
-- eg. [(1+), (5*), (10*)] <*> [1,2,3]
-- ans. [2,3,4,5,10,15,10,20,30]

-- finding every pairwise product of two lists
-- pure (*) <*> [1,2,3] <*> [10,20,30]
-- [10,20,30,20,40,60,30,60,90]

-- ZipList [(1+), (5*), (10*)] <*> [5,10,15]
-- output: ZipList {getZipList = [6,50,150]}
