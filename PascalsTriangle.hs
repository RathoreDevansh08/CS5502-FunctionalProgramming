-- Enter your code here. Read input from STDIN. Print output to STDOUT
module Main where

cho n r = (factorial n) `div` ((factorial (r)) * (factorial (n-r)))

pri n = print . (pas 1)

main = do
    input <- getLine
    pri . (read :: String -> Int) $ input
