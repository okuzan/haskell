{-# OPTIONS_GHC -Wall #-}
module Kuzan03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving (Show, Eq)
type Program = [Command]
type ConfigС = (Int, Int, [Int])

first::Substitution -> String
first (a,_,_) = a
second::Substitution -> String
second (_,a,_) = a
third::Substitution -> Bool
third (_,_,a) = a

first'::ConfigA -> Bool
first' (a,_,_) = a
second'::ConfigA -> Int
second' (_,a,_) = a
third'::ConfigA -> String
third' (_,_,a) = a

first''::ConfigС -> Int
first'' (a,_,_) = a
second''::ConfigС -> Int
second'' (_,a,_) = a
third''::ConfigС -> [Int]
third'' (_,_,a) = a

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix [] [] = True
isPrefix _ [] = False
isPrefix [] _ = True
isPrefix (p:ps) (s:ss) = p == s && (isPrefix ps ss)

-- Задача 2 ------------------------------------


substitute :: Substitution -> Int -> String -> String
substitute sub x str = let a = drop x str
                           b = take x str
                           c = first sub
                           in if c `isPrefix` a then b ++ second sub ++ drop (length c) a  else str

-- Задача 3------------------------------------
findPosition' :: String -> Substitution -> [(Substitution,Int)] 
findPosition' [] ([],b, True) =[(([],b,True),0)]
findPosition' [] ([],b, False) =[(([],b,False),0)]
findPosition' [] (_,_,True) = []
findPosition' [] (_,_,False) = []
findPosition' s@(_:str) sub = if first sub `isPrefix` s
                              then [(sub, 0 - length str - 1)] ++ (findPosition' str sub) else findPosition' str sub

findPosition ::  String -> Substitution -> [(Substitution,Int)] 
findPosition str sub = let res = (findPosition' str sub)
                       in [(fst s, (length str) + (snd s)) | s <- res]

-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution, Int)]
findAll alg str = concatMap (findPosition str) alg

-- Задача 5 ------------------------------------

stepA :: Algorithm -> ConfigA -> ConfigA
stepA [] a = a
stepA (x:algo) conf = if first' conf /= False
                        then if findPosition (third' conf) x /= []
                             then (not (third x), (second' conf) + 1, 
                             substitute x (snd (head (findPosition (third' conf) x))) (third' conf))
                             else stepA algo conf
                        else conf

-- Задача 6 ------------------------------------
-- evalA :: Algorithm -> Int -> String -> Maybe String 
-- evalA algo tops str =  if first' (stepA algo (True, 0, str)) == False
--                       then Just third' stepA algo (True, 0, str)
--                       else evalA algo tops (third' stepA algo (True, 0, str))

ccc :: Algorithm -> Int -> String -> ConfigA
ccc algo tops str = until (check tops) (stepA algo) (True, 0, str)

evalA :: Algorithm -> Int -> String -> Maybe String
evalA algo tops str = let res = ccc algo tops str
                    in if first' res == True then Nothing else Just (third' res)

check:: Int -> ConfigA -> Bool
check tops conf = second' conf == tops || first' conf == False

-- Задача 7 ------------------------------------
comToList :: Command -> [Int]
comToList p = case p of
   J a b _ ->  [a,b]
   S a     ->  [a]
   Z a     ->  [a] 
   T a b   ->  [a,b]

maximReg :: Program -> Int
maximReg prog = maximum $ concatMap comToList prog
              

-- Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int] 
ini prog xs = xs ++ replicate (maximReg prog - length xs) 0

upd :: [Int] -> Int -> Int-> [Int]
upd xs n el = take n xs ++ [el] ++ drop (n + 1) xs 

-- Задача 9 ------------------------------------
spell :: Command -> ConfigС -> (Int, [Int])
spell com conf = case com of
                            S a -> let xs = third'' conf in (first'' conf + 1, take (a - 1) xs ++ [(xs !! (a - 1)) + 1] ++ drop a xs)
                            Z a ->  let xs = third'' conf in (first'' conf + 1, take (a - 1) xs ++ [0] ++ drop a xs)
                            T a b -> let xs = third'' conf in (first'' conf + 1, take (b - 1) xs ++ [xs !! (a - 1)] ++ drop b xs)
                            J a b c -> if (third'' conf) !! (a - 1) == (third'' conf) !! (b - 1) 
                                       then (c, third'' conf)
                                       else (first'' conf + 1,  third'' conf)
-- |otherwise (0, [])


stepC :: Program -> ConfigС -> ConfigС
stepC prog conf = (conNum, (1 + second'' conf), vals) where conNum = fst (spell (prog !! (first'' conf - 1)) conf)
                                                            vals   = snd (spell (prog !! (first'' conf - 1)) conf)

-- Задача 10 ------------------------------------

bbb :: Program -> Int -> [Int] -> ConfigС
bbb prog tops inl = until (checker tops (length prog)) (stepC prog) (1, 0, inl)

evalC :: Program -> Int -> [Int] -> Maybe Int
evalC prog tops inl = let res = bbb prog tops inl
                    in if first'' res > length prog then Just (head (third'' res)) else Nothing

checker:: Int -> Int -> ConfigС -> Bool
checker tops len conf = second'' conf == tops || (first'' conf) > len 

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]
