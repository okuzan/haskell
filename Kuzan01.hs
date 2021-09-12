{-# OPTIONS_GHC -Wall #-}
module Kuzan01 where

-- Çàäà÷à 1 -----------------------------------------
power3 :: [Integer]
power3 = [x*x*x | x <- [1..]]

-- Çàäà÷à 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [3^x | x <- [1::Int,2..]]

-- Çàäà÷à 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n = sum (take (fromInteger n) toPower3)

-- Çàäà÷à 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum ([m^x | x <- [1::Int, 2 .. fromInteger n]])

-- Çàäà÷à 5 -----------------------------------------
lessMeUtil :: Int -> [Int] -> Int
lessMeUtil x xs = sum([if x > a then 1 else 0 | a <- xs])

lessMe :: [Int] -> [Int]
lessMe xs = [ lessMeUtil x xs| x <- xs] 
 
-- Çàäà÷à 6 -----------------------------------------
frequencyUtil :: Int -> [Int] -> Int
frequencyUtil x xs = sum([if x == a then 1 else 0 | a <- xs])

frequency0 :: [Int] -> [(Int,Int)]
frequency0 xs = myNub [ (x, (frequencyUtil x xs)) | x <- xs]

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p xs = [x | x <- xs, p x]  

myNub :: (Eq a) => [a] -> [a]
myNub (x:xs) = x : myNub (filter2 (/= x) xs)
myNub [] = []

-- Çàäà÷à 7 -----------------------------------------
hailstone :: Int -> Int
hailstone x = if even x then x `div` 2 else  3*x + 1

-- Çàäà÷à 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq 1 = [1]
hailSeq x = [x] ++ hailSeq(hailstone x)


-- Çàäà÷à 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <-[1::Int,2..]]

-- Çàäà÷à 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq x = until (check x) (+1) 1

check :: Int -> Int -> Bool
check a b = length (hailSeq b) == a

-- firstHailSeq x = sum([if length s /= x then 1 else | s <- allHailSeq])
