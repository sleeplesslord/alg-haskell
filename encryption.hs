import Data.List

chop :: Int -> String -> [String]
chop _ [] = []
chop n xs = take n ys : chop n (drop n xs)
    where ys = xs

isNotSpace x = x /= ' '

solve xs = init . concat . map (++ " ") . transpose $ chop ceil xs
    where
        l = fromIntegral $ length xs
        fl = floor $ sqrt l
        ceil = ceiling $ sqrt l

main = interact $ solve . filter isNotSpace 
