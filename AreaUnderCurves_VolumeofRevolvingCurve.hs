import Text.Printf (printf)

solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [area, volume] where
    h = 0.001
    ln = fromIntegral l
    rn = fromIntegral r    
    heights = [ sum ([i * (x**j) | (i, j) <- zip (map fromIntegral a) ( map fromIntegral b)]) | x <- [ln, ln+h .. rn]]
    area = h * sum (heights)
    volume = h * pi *  sum [ h**2 | h <- heights]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
