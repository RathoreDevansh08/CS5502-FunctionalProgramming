class Monad m where  
   return :: a -> m a 
   (>>=) :: m a -> (a -> m b) -> m b 
   (>>) :: m a -> m b -> m b 
   x >> y = x >>= \_ -> y 
   fail :: String -> m a  
   fail msg = error msg

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  Just a >>= f = f a

main = do
   print([1..10] >>= (\x -> if odd x then [x*2] else []))
-- output: [2,6,10,14,18]

maybeFunc1 :: String -> Maybe Int
maybeFunc1 “” = Nothing
maybeFunc1 str = Just $ length str

maybeFunc2 :: Int -> Maybe Float
maybeFunc2 i = if i `mod` 2 == 0
  then Nothing
  Else Just ((fromIntegral i) * 3.14159)

maybeFunc3 :: Float -> Maybe [Int]
maybeFunc3 f = if f > 15.0
  then Nothing
  else $ Just [floor f, ceil f]

runMaybeFuncs :: String -> Maybe [Int]
runMaybeFuncs input = maybeFunc1 input >>= maybeFunc2 >>= maybeFunc3

-- runMaybeFuncs "Hit"   outputs>   Just [9,10]
