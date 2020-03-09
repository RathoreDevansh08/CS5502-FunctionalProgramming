takeme n xs | n <= 0 = []
            | otherwise = case xs of
                        u:us -> u : takeme (n-1) us
                        _ -> []

main = do
    let s = takeme 2 [3..]
    putStrLn "Its me"
