module Kuzan06 where
import Data.List

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- Задача 1 ------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary gr = if null gr then False else checkApex (zip [0..] gr) gr

checkApex :: [(Int, [Int])] -> Graph -> Bool
checkApex [] _ = True
checkApex ((id, body):gr) fullGr = not $ False `elem` [not $ (id `elem` body), isUniq body,
                                                       checkEdge body id fullGr, checkApex gr fullGr]
isUniq :: [Int] -> Bool
isUniq xs = length (nub xs) == length xs

checkEdge ::  [Int] -> Int -> Graph -> Bool
checkEdge [] _ _ = True
checkEdge (x:body) id gr = id `elem` (gr !! x) && checkEdge body id gr


-- Задача 2 ------------------------------------
fromGraph :: Graph -> GraphS 
fromGraph gr = (length gr - 1, listEdges (zip [0..] gr) [])

listEdges ::  [(Int, [Int])] -> [(Int, Int)] -> [(Int, Int)]
listEdges [] res = res
listEdges (inList:gr) res = listEdges gr (res ++ (unfold inList))

unfold :: (Int, [Int]) -> [(Int, Int)]
unfold (id, []) = []
unfold (id, (x:xs)) = [(id, x)] ++ (unfold (id, xs))

-- Задача 3 ------------------------------------
toGraph :: GraphS -> Graph 
toGraph grs = let n = fst grs
                  body = snd grs
              in snd $ unzip $ fillB body (zip [0..] (createMold n))

fillB :: [(Int, Int)] -> [(Int, [Int])] -> [(Int, [Int])]
fillB [] a = a
fillB (l:lst) mold = fillB lst [if fst x == fst l then (fst x, snd x ++ [snd l]) else x | x <- mold ]


createMold n = let lst0 = take (n + 1) (repeat "a") in [ [] | x <- lst0]

-- Задача 4 ------------------------------------
shortWay :: Graph -> Int -> Int -> [Int] 
-- shortWay gr a b =  let res = until (isFound b) (stepW gr) [[a]]
                   -- in if null res then [] 
                      -- else reverse $ head [a | a<-res, b `elem` a]

shortWay gr a b = head $ shortWays gr a b 
isFound:: Int -> [[Int]] -> Bool
isFound b lst = null lst || True `elem` [b `elem` x | x <- lst]

stepW :: Graph -> [[Int]] -> [[Int]]
stepW gr xs = concatMap (appUtil gr) xs


appUtil :: Graph -> [Int] -> [[Int]]
appUtil gr xs = let paths = [ a | a <- (gr !! head xs), not $ a `elem` xs]
                in if null paths then []
                   else createVars paths xs []


createVars :: [Int] -> [Int] -> [[Int]] -> [[Int]]
createVars [] _ r = r
createVars (p:paths) xs res = createVars paths xs res ++ [p:xs] 

-- Задача 5 ------------------------------------
isConnecting :: Graph -> Bool 
isConnecting gr = not $ null $
                  until (\l -> l == [] || sort (nub( concat l)) == [0..(length gr -1)]) 
                  (stepW gr) [[0]]

-- Задача 6 ------------------------------------
components :: Graph -> [[Int]] 
components gr = filter (/= []) $ compUtil gr 0 [0..(length gr - 1)] [[]]

compUtil :: Graph -> Int -> [Int] -> [[Int]] -> [[Int]]
compUtil gr a freeApexes comps = let oneRun = maxGrasp gr a 
                                     left = freeApexes \\ oneRun
                                 in  if null left then comps ++ [oneRun]
                                     else compUtil gr (head left) left (comps ++ [oneRun])

maxGrasp :: Graph -> Int -> [Int]
maxGrasp gr a = sort $ fst $ until (\(_, lst) -> null lst) (stepM gr) ([a], [[a]])

stepM :: Graph -> ([Int], [[Int]]) -> ([Int], [[Int]])
stepM gr (uniq, xs) = let res = concatMap (appM gr) xs
                      in (uniq ++ ((nub $ concat $ res) \\ uniq), res)


appM :: Graph -> [Int] -> [[Int]]
appM gr xs = let paths = [ a | a <- (gr !! head xs), not $ a `elem` xs]
                in if null paths then []
                   else createVars paths xs []


-- Задача 7 ------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity gr a = (maximum $ concatMap (\x -> [length $ shortWay gr a x]) [0..(length gr - 1)]) - 1

eccUtil gr a x = [length $ shortWay gr a x]

-- Задача 8 ------------------------------------
findDiameter :: Graph -> Int 
findDiameter = maximum . listEcc

findRadius :: Graph -> Int 
findRadius = minimum . listEcc

listEcc :: Graph -> [Int]
listEcc gr = concatMap (\x -> [eccentricity gr x]) [0..(length gr - 1)]

-- Задача 9 ------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr = let lst = zip [0..(length gr - 1)] (listEcc gr) 
                    min = findRadius gr
                    mins = filter (\(a, b) -> b == min) lst
                in [fst a | a <- mins]

-- Задача 10 ------------------------------------
shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr a b = let res = until (isFound b) (stepW gr) [[a]]
                   in if null res then [] 
                      else sort $ map (reverse) [a | a <- res, b `elem` a]

---------------------Тестові дані - Графи -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]
