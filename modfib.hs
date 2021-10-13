sumSquare a b = a + b * b

modfib :: [Integer] -> [Integer]
modfib start = start ++ zipWith (sumSquare) f (tail f)
  where f = modfib start

nthFib :: [Integer] -> Integer
nthFib s = last $ take (fromIntegral $ last s) $ (modfib (init s))

main :: IO()
main = interact $ show . nthFib . map read . words 
