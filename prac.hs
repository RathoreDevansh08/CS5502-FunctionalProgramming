module List where

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (a:as) (b:bs) = f a b : (myZipWith f as bs)
myZipWith f _      _      = []

-- takes maximum of first n elements from given list
mytake :: Int -> [a] -> [a]
mytake 0 (a:as) = []
mytake _ [] = []
mytake n (a:as) = a : mytake (n-1) as

-- drops n elements from starting of list
mydrop :: Int -> [a] -> [a]
mydrop 0 [a] = [a]
mydrop _ []  = []
mydrop n (a:as) = mydrop (n-1) as

-- returns list without first element of list
mytail :: [a] -> [a]
mytail [] = []
mytail (a:as) = as

-- fibonacci (using lazy evaluation)
fibs = mytake 10 (1 : 1 : myZipWith (+) fibs (mytail fibs))

-- x gives [1, 1, 1, .....] (using lazy evaluation)
x = 1 : x
y = mytake 10 x

-- filter functions gives shorter list of values that satisfy given function in a list
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter pred [] = []
myfilter pred (a:as) | pred a = a : myfilter pred as
                     | otherwise = myfilter pred as

-- Sieve
mySieve :: [Int] -> [Int]
mySieve (x:xs) = x : mySieve (myfilter pred xs)
                 where
                      pred t = t `mod` x /= 0

-- Primes (first n)
myprimes :: Int -> [Int] -> [Int]
myprimes n xs = mytake n (mySieve xs)                           

