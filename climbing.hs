import Data.Array

dedup :: Eq a => [a] -> [a]
dedup [x] = [x]
dedup [] = []
dedup (x:y:xs)
    | x == y    =  dedup (x:xs)
    | otherwise = x :  dedup (y:xs)

binarySearch :: Array Int Int -> (Int, Int) -> Int -> Int
binarySearch haystack (start, end) needle
    | start == end = start + lastPlace
    | order == GT = binarySearch haystack (start, middleIndex) needle 
    | order == LT = binarySearch haystack (middleIndex + 1, end) needle
    | order == EQ = middleIndex
    where
        middleIndex = (start + end) `div` 2
        middle = haystack ! middleIndex
        order = compare needle middle
        lastPlace = if order == LT && end == (length haystack) - 1 then 1 else 0

solve :: [Int] -> [Int]
solve (n:rest) = map ((+1) . binaryFun) player
    where
        ranked = dedup $ take n rest
        rankedArr = array (0, (length ranked) - 1) [(i, x) | (i, x) <- zip [0..] ranked]
        binaryFun = binarySearch rankedArr (0, (length ranked) - 1)
        player = tail $ drop n rest

main = interact $ unlines . map show . solve . map read . words
