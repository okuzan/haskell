{-# OPTIONS_GHC -Wall #-}
module Kuzan02 where

-- ������ 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl = foldl (+) 0
  
-- ������ 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr = foldr (*) 1

-- ������ 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs

-- ������ 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert [] el = [el]
insert (x:xs) el
    | el <= x = el : x : xs
    | otherwise = x : (insert xs el)


sortInsert :: [Int] -> [Int]
sortInsert [] = []
sortInsert (x:xs) = foldl insert [x] xs

-- ������ 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices v xs = fff (zip xs [0..]) v

fff :: [(Int, Int)] -> (Int -> Bool)-> [Int]
fff xs y = [snd x | x <- xs, y (fst x)]

-- ������ 6 -----------------------------------------
revUtil :: [String] -> [String]
revUtil [] = []
revUtil (x:xs)  = allReverse xs ++ [x]

allReverse :: [String] -> [String]
allReverse xs = map reverse (allReverse xs)

-- ������ 7  -----------------------------------------
noDigits :: String -> String
noDigits xs = filter (not . (`elem` ['0'..'9'])) xs

-- ������ 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v = sum [1 | x <- ps, x v]

-- ������ 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate (\x ->zipWith (+) ([0] ++ x)(x ++ [0])) [1]

-- ������ 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : zipWith (*) factorialsM [2..]