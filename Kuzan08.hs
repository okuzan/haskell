-- # OPTIONS_GHC -Wall #
module Kuzan08 where
import Data.List
import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec ((<|>))



data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  

-- Çàäà÷à 1 ------------------------------------
-- parse3 rule text = Parsec.parse "" rule text

isNumbConst :: System -> Recur -> Bool 
isNumbConst _ Zero = True
isNumbConst sys (Super Succ [f]) = isNumbConst sys f
isNumbConst sys (Name name) = let a = getByName sys name in if null a then False else isNumbConst sys $ head a
isNumbConst _ _ = False

findName :: System -> String -> Bool
findName sys str = not $ null $ filter (\(a, _) -> a == str) sys

getByName :: System -> String -> [Recur]
getByName sys str = if null found then [] else [snd $ head found]
                    where found = filter (\(a, _) -> a == str) sys

-- Çàäà÷à 2 ------------------------------------
evRank :: System -> Recur -> Int 
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n _ ) = n
evRank sys (Super _ xs) = evRank sys $ head xs
evRank sys (Name str) = evRank sys $ head $ getByName sys str
evRank sys (Prim _ rec) = (evRank sys rec) - 1
evRank sys (Mini rec _) = (evRank sys rec) - 1

-- Çàäà÷à 3 ------------------------------------
isNames :: System -> Bool 
isNames sys = noDupes sys && checkDefin [] sys

checkDefin :: System -> System -> Bool  
checkDefin    _   [] = True
checkDefin prev left = (and $ map (\str -> not $ null [x | x <- prev, fst x == str]) 
                            $ filter (/= "")   $ getIfName (snd $ head left)) 
                        && checkDefin (prev ++ [head left]) (tail left)

getIfName :: Recur -> [String]
getIfName (Name str) = [str]
getIfName (Super a xs) = getIfName a ++ concatMap (getIfName) xs
getIfName (Prim a b) = getIfName a ++ getIfName b
getIfName (Mini a _) = getIfName a
getIfName _ = [""]

noDupes :: Eq a => [(a, b)] -> Bool
noDupes xs = length xs == length (nubBy (\(p, _) (r, _) -> p == r) xs)

-- Çàäà÷à 4 ------------------------------------

isRecur :: System -> Recur -> Bool
isRecur _ Zero = True
isRecur _ Succ = True
isRecur _ (Sel n k) = n >= 1 && k >= 1 && k <= n
isRecur sys (Super a xs) =      evRank sys a == length xs 
                           && (allTheSame $ map (evRank sys) xs)
                           && (and $ map (isRecur sys) xs)
                           && isRecur sys a
isRecur sys rec@(Name str) = let res = getByName sys str in if null res then False else isRecur sys $ head res
isRecur sys (Prim g h) = let rH = evRank sys h 
                             rG = evRank sys g 
                             flag = isRecur sys g && isRecur sys h
                             ifGT = rG == (rH - 2)
                             ifEQ = rG == 1 && isNumbConst sys g
                         in case () of 
                         _ | not flag  -> False
                           | rH > 2    -> ifGT 
                           | rH == 2   -> ifEQ
                           | otherwise -> False

isRecur sys (Mini g t) = isRecur sys g && evRank sys g > 1 && t >= 0

allTheSame xs = and $ map (== head xs) (tail xs)

-- Çàäà÷à 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int 
eval _ Zero _ = 0
eval _ Succ x = (head x) + 1 
eval _ (Sel _ k) xs = xs !! (k - 1)
eval sys (Name str) xs = eval sys (head $ getByName sys str) xs
eval sys rec@(Super g hs) xs = eval sys g $ map (\h -> eval sys h xs) hs
-- eval sys rec@(Prim g h)  xs = if last xs == 0 then eval sys g (init xs) 
--                               else eval sys h (init xs ++ [(last xs - 1)]
--                                                        ++ [eval sys rec (init xs ++ ([last xs - 1]))])
eval syst pr@(Prim g h) vl
         | (last vl) == 0 = eval syst g (init vl)
         | otherwise      = eval syst h (init vl ++ [(last vl - 1)] ++ [eval syst pr (init vl ++ [(last vl) - 1])])

-- Çàäà÷à 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int 
evalPart _ Zero _ = Just 0
evalPart _ Succ x = Just $ (head x) + 1 
evalPart _ (Sel _ k) xs = Just $ xs !! (k - 1)
evalPart sys (Name str) xs = evalPart sys (head $ getByName sys str) xs
evalPart sys (Mini g t) xs = (getYmini sys g t xs 0)
evalPart sys rec@(Super g hs) xs = let res = map (\h -> evalPart sys h xs) hs
                                   in case () of
                                    _ | Nothing `elem` res -> Nothing
                                      | otherwise -> evalPart sys g $ map unfold res
evalPart sys rec@(Prim g h)  xs = case () of
                                  _ | last xs == 0 -> evalPart sys g (init xs)
                                    | res /= Nothing -> evalPart sys h (init xs ++ [(last xs - 1)] ++ [unfold res])
                                    | otherwise -> Nothing
                                  where res = evalPart sys rec (init xs ++ ([last xs - 1]))

unfold :: Maybe Int -> Int
unfold (Just a) = a

getYmini :: System -> Recur -> Int -> [Int] -> Int -> Maybe Int
getYmini sys g t xs y
         | y > t = Nothing
         | r == Just 0 = Just y
         | ((Just (> 0)) <*> r) == Just True = getYmini sys g t xs (y + 1)
         | otherwise = Nothing
         where r = evalPart sys g (xs ++ [y])


cre = all (==True) (map (isRecur syst1) $ map (\(a, b) -> b) syst1)

-- Çàäà÷à 7 ------------------------------------
parseRec :: String -> Maybe System 
parseRec s = let res = parse system "" (filter (not . isSpace) s) in case res of 
  Right a  -> Just a
  Left _   -> Nothing
 
pInt :: Parser Int
pInt = foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

selec :: Parser Recur
selec = do
    char 's'
    n <- digit
    k <- digit
    return (Sel (digitToInt n) (digitToInt k))

zero :: Parser Recur
zero = string "z1" >> return Zero

succer :: Parser Recur
succer = string "a1" >> return Succ

name :: Parser Recur
name = do
  str <- many $ alphaNum 
  return (Name str)

recur :: Parser Recur
recur = prim <|> super <|> mini <|> base 

base :: Parser Recur
base = try zero <|> try selec <|> try succer <|> name

prim :: Parser Recur
prim = do
   char '['
   rec1 <- recur
   char ','
   rec2 <- recur
   char ']'
   return $ Prim rec1 rec2

superlst :: Parser [Recur]
superlst = many $ do
    char ','
    rec <- recur
    return rec

iden :: Parser String
iden = do
  c <- letter
  s <- many $ letter <|> digit
  return $ c:s

mini :: Parser Recur
mini = do
   char '{'
   g <- recur
   char ','
   t <- pInt
   char '}'
   return $ Mini g $ t

pairCheck :: Parser (String, Recur)
pairCheck = do   
   name <- many alphaNum   
   char '='   
   rec <- recur   
   char ';'  
   return (name, rec)

system :: Parser [(String, Recur)]
system = do  
  pair <- many pairCheck  
  return pair

super :: Parser Recur
super = do
   char '('   
   rec <- recur   
   char ':'         
   rec1 <- recur   
   lst <- superlst   
   char ')'
   return $ Super rec (rec1:lst)


---------------------Òåñòîâ³ äàí³ -  -------
syst, syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
syst = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]

syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"]) 
        ]


sysStr1,sysStr2 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	  \  notSignum = [(a1:z1),(z1:s21)];\n\
	  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
	  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"
 
sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
