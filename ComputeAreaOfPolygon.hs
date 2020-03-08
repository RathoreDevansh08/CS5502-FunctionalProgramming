-- Enter your code here. Read input from STDIN. Print output to STDOUT
getTuple :: [Float] -> (Float, Float)

getTuple coord = (head coord, last coord)


getFirst2SecondArraySum :: (Float, Float) -> (Float, Float) -> Float
getSecond2FirstArraySum :: (Float, Float) -> (Float, Float) -> Float

getFirst2SecondArraySum c1 c2 = fst c1 * snd c2
getSecond2FirstArraySum c1 c2 = fst c2 * snd c1

getFirstSecondDiff :: (Float, Float) -> (Float, Float) -> Float

getFirstSecondDiff c1 c2 = getFirst2SecondArraySum c1 c2 - getSecond2FirstArraySum c1 c2

getSumDiff :: [(Float,Float)] -> Float

getSumDiff (a1:a2:as)
    | null (a1:a2:as) = 0
    | null as = getFirstSecondDiff a1 a2
    | otherwise = getFirstSecondDiff a1 a2 + getSumDiff (a2:as)

getArea :: [(Float, Float)] -> Float

getArea (c:coords) = let newcoords = (c:coords) ++ [c]
                        in getAbsVal ((getSumDiff newcoords) * 0.5)


getAbsVal :: Float -> Float

getAbsVal n
    | n > 0 = n
    | otherwise = -n

readWords :: String -> [Float]
readWords str = map read (words str) :: [Float]

-- input output
main = do
  n <- readLn :: IO Int
  inputdata <- getContents 
  let coordinates = map readWords (lines inputdata)
    in print  (getArea (map getTuple coordinates))
