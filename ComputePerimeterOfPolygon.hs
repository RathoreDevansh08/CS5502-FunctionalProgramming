-- Enter your code here. Read input from STDIN. Print output to STDOUT
perimeter ls = sum [distance p q | (p, q) <- (zipW . makeCircular . zipV) ls]

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x, y) (u, v) = sqrt (fromIntegral ((x - u)^2 + (y - v)^2)) 

zipV :: [a] -> [(a, a)]
zipV [] = []
zipV (x:y:ls) = ((x, y):(zipV ls))

zipW :: [a] -> [(a, a)]
zipW (x:ls) = zip (x:ls) ls

makeCircular :: [a] -> [a]
makeCircular ls = [last ls] ++ ls

main :: IO ()
main = do
    _ <- getLine
    x <- getContents
    let f = map (read::String->Int) $ words x
    print (perimeter f)
