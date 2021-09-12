{-# OPTIONS_GHC -Wall #-}
module Kuzan04 where

import Data.Char(digitToInt)

-- Задача 1 -----------------------------------------
analyseG :: String -> Bool 
analyseG st1 = case s st1 of
    Just st2 -> null st2
    Nothing  -> False

s :: String -> Maybe String
s ('a':st1) = case s st1 of
    Just ('b':st2) -> case a st2 of
        Just ('a':st3) -> Just st3
        _ -> Nothing
    _ -> Nothing
s ('b':st1) = Just st1
s _ = Nothing

a :: String -> Maybe String
a ('b': st1) = case a st1 of
    Just ('a':st2) -> s st2
    _ -> Nothing
a ('a':st1) = Just st1
a _ = Nothing
   
-- Задача 2 ----------------------------------------
balance :: String -> Bool
balance = (maybe False null) . b

b :: String -> Maybe String 
b st = case c st of
    Just st2 -> e st2
    _ -> Nothing

c :: String -> Maybe String 
c (' ':st1) = c st1
c st = Just st

e :: String -> Maybe String 
e ('(':st1) = case b st1 of
    Just (')':st2) -> b st2
    _ -> Nothing
e ('[':st1) = case b st1 of
    Just (']':st2) -> b st2
    _ -> Nothing
e ('{':st1) = case b st1 of
    Just ('}':st2) -> b st2
    _ -> Nothing
e st = Just st


ss :: String -> Maybe String 
ss ('(':st1) = case ss st1 of        -- S -> (S)S 
      Just (')':st2) -> ss st2 
      _                -> Nothing  
ss st        = Just st                   -- S -> ε 

analyseB :: String -> Bool 
analyseB = (maybe False null) . ss

-- Задача 3 -----------------------------------------
analyseExpr :: String -> Bool 
analyseExpr = (maybe False null) . ae

ae :: String -> Maybe String 
ae st = case af st of
    Just st2 -> aa st2
    _ -> Nothing

aa :: String -> Maybe String 
aa (x:st1) | x `elem` "-+*" = case af st1 of 
                          Just st2 -> aa st2
                          _ -> Nothing
aa st = Just st

af :: String -> Maybe String 
af ('(':st1) = case ae st1 of
    Just (')':st2) -> Just st2
    _ -> Nothing
af (x:st1) | x `elem` "0123456789" = Just st1
af _ = Nothing


-- Задача 4 -----------------------------------------
evalLeft :: String -> Maybe Int 
evalLeft st1 = case le st1 of 
            Just (v, st2) | null st2 -> Just v 
            _                        -> Nothing

le :: String -> Maybe (Int, String)
le st = case lf st of
    Just (v, st2) -> la (v, st2)
    _ -> Nothing

la :: (Int, String) -> Maybe (Int, String) 
la (v, x:st1) | x `elem` "*" = case lf st1 of 
                          Just (v1, st2) -> la (v*v1, st2)
                          _ -> Nothing
la (v, x:st1) | x `elem` "-" = case lf st1 of 
                          Just (v1, st2) -> la (v-v1, st2)
                          _ -> Nothing
la (v, x:st1) | x `elem` "+" = case lf st1 of 
                          Just (v1, st2) -> la (v+v1, st2)
                          _ -> Nothing

la (v, st) = Just (v, st)

lf :: String -> Maybe (Int, String)
lf ('(':st1) = case le st1 of
    Just (v1,')':st2) -> Just (v1, st2)
    _ -> Nothing
lf (x:st1) | x `elem` "0123456789" = Just (digitToInt x, st1)
lf _ = Nothing


-- Задача 5 -----------------------------------------
evalRigth :: String -> Maybe Int 
evalRigth st1 = case re st1 of 
            Just (v, st2) | null st2 -> Just v 
            _                        -> Nothing

re :: String -> Maybe (Int, String)
re st = case rf st of
    Just (v, st2) -> ra (v, st2)
    _ -> Nothing

ra :: (Int, String) -> Maybe (Int, String) 
ra (v, x:st1) | x `elem` "-+*" = case re st1 of 
                          Just (v1, st2) -> Just (inOp x v v1, st2)
                          _ -> Nothing
-- ra (v, x:st1) | x `elem` "-" = case re st1 of 
--                           Just (v1, st2) -> Just (v-v1, st2)
--                           _ -> Nothing
-- ra (v, x:st1) | x `elem` "+" = case re st1 of 
--                           Just (v1, st2) -> Just (v+v1, st2)
--                           _ -> Nothing
ra (v, st) = Just (v, st)

rf :: String -> Maybe (Int, String)
rf ('(':st1) = case re st1 of
    Just (v1,')':st2) -> Just (v1, st2)
    _ -> Nothing
rf (x:st1) | x `elem` "0123456789" = Just (digitToInt x, st1)
rf _ = Nothing

-- Задача 6 -----------------------------------------
evalPrior :: String -> Maybe Int 
evalPrior st1 = case pe st1 of 
            Just (v, st2) | null st2 -> Just v 
            _                        -> Nothing

pe :: String -> Maybe (Int, String) 
pe st = case pt st of
    Just (v, st2) -> pa (v, st2)
    _ -> Nothing

pa :: (Int,String) -> Maybe (Int,String) 
pa (v, x:st1) | x `elem` "-+" = case pt st1 of 
                          Just (v1, st2) -> pa (inOp x v v1, st2)
                          _ -> Nothing
-- pa (v, x:st1) | x `elem` "+" = case pt st1 of 
--                           Just (v1, st2) -> pa (v+v1, st2)
--                           _ -> Nothing
pa (v, st) = Just (v, st)

pt :: String -> Maybe (Int, String) 
pt st = case pf st of
    Just (v, st2) -> pb (v, st2)
    _ -> Nothing

pb :: (Int,String) -> Maybe (Int,String) 
pb (v, x:st1) | x `elem` "*" = case pf st1 of 
                          Just (v1, st2) -> pb (v*v1, st2)
                          _ -> Nothing
pb (v, st) = Just (v, st)

pf :: String -> Maybe (Int, String) 
pf ('(':st1) = case pe st1 of
    Just (v1,')':st2) -> Just (v1, st2)
    _ -> Nothing
pf (x:st1) | x `elem` "0123456789" = Just (digitToInt x, st1)
pf _ = Nothing

------------------------------------------------------
match :: Char -> Maybe String -> Maybe String 
match c1 (Just (t:st)) | c1==t = Just st
match _ _                      = Nothing 

inOp :: Char -> Int -> Int -> Int
inOp c2 = case c2 of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}