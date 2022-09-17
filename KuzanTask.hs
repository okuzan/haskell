-- {-# OPTIONS_GHC -Wall #-}
module KuzanTask where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

type Graph  = [[Int]]

data Tree23 a  = Leaf a   
               | Fork2 (Tree23 a) a (Tree23 a) 
               | Fork3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Null23     -- порожнє 2-3-дерево!!!
               deriving (Eq, Show)

data Expr  = Val Int | App Op Expr Expr 
data Op    = Add | Sub | Mul 
type Result = (Expr, Int)


instance Show Op where 
  show Add = "+"
  show Sub = "-"
  show Mul = "*" 

instance Show Expr where 
  show (Val n) = show n 
  show (App op e1 e2) = "(" ++ (show e1) ++ show op ++ show e2 ++ ")"

-- Задача 1 -----------------------------------------
instance Ord AbstractInteger where (<=) Zero Zero = True
                                   (<=) (Succ a) (Succ b) = a <= b
                                   (<=) (Pred a) (Pred b) = a <= b
                                   (<=) (Succ a) Zero = a < Zero
                                   (<=) (Pred a) Zero = Succ Zero >= a
                                   (<=) Zero (Succ a) = Pred Zero <= a
                                   (<=) Zero (Pred a) = Zero < a
                                   (<=) (Pred a) (Succ b) = (Succ b) >= (Pred a)
                                   (<=) (Succ a) (Pred b) = a < b
   
-- Задача 2 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger Zero = 0
aiToInteger (Pred a) = (aiToInteger a) - 1 
aiToInteger (Succ a) = (aiToInteger a) + 1 
 
-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero b = b
plusAbs (Succ a) b = plusAbs a (Succ b)
plusAbs (Pred a) b = plusAbs a (Pred b)

negateAbs :: AbstractInteger -> AbstractInteger 
negateAbs Zero = Zero
negateAbs (Succ a) = Pred (negateAbs a)
negateAbs (Pred a) = Succ (negateAbs a)

-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs Zero _ = Zero
timesAbs _ Zero = Zero
timesAbs (Succ a) b = plusAbs b $ timesAbs a b
timesAbs (Pred a) b = plusAbs (negateAbs b) $ timesAbs a b

-- Задача 5 -----------------------------------------
instance Num AbstractInteger  where
    (+)   = plusAbs
    (*)   = timesAbs
    negate      = negateAbs
    fromInteger x
     | x == 0 = Zero
     | x < 0 = Pred (fromInteger $ x + 1)
     | otherwise = Succ (fromInteger $ x - 1)
    abs a       = case a of
        (Pred x)  -> negateAbs x
        x -> x 
    signum   a = case a of
        Zero     -> Zero
        (Pred _) -> (Pred Zero)
        (Succ _) -> (Succ Zero)
 
-- Задача 6 -----------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr = gamApply vars
  where s = maximum $ concat gr
        vars = myNub $ map (normalize) $ perm [0..s]
        gamUtil [] = True
        gamUtil [_] = True
        gamUtil (x:y:xs) = if y `elem` (gr !! x) then gamUtil (y:xs) else False
        gamApply [] = Nothing
        gamApply (c:cs) = case res of
                           False -> gamApply cs
                           True  -> Just c
                          where res = gamUtil c

myNub :: (Eq a) => [a] -> [a]
myNub (x:xs) = x : myNub (filter (/= x) xs)
myNub [] = []

perm :: [a] -> [[a]]
perm []     = return []
perm (x:xs) = (perm xs) >>= (ins x)
    where
    ins :: a -> [a] -> [[a]]
    ins k []     = [[k]]
    ins k (y:ys) = [k:y:ys] ++ ( map (y:) (ins k ys) )

normalize :: [Int] -> [Int]
normalize xs = let (a,b) = saf 0 xs
               in 0:b ++ a ++ [0]

saf :: Eq a => a -> [a] -> ([a], [a])
saf x = fmap (drop 1) . break (x ==) 

-- Задача  7 -----------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic gr = and $ map (acycl) [0..s]
              where s = maximum $ concat gr
                    acycl a = null $ until (isFound) (stepW gr) [[a]]  

isFound:: [[Int]] -> Bool
isFound lst = null lst || True `elem` [not $ allDifferent x | x <- lst]

stepW :: Graph -> [[Int]] -> [[Int]]
stepW gr xs = concatMap (appUtil gr) xs

appUtil :: Graph -> [Int] -> [[Int]]
appUtil gr (xs) = let paths = [ a | a <- (gr !! head xs)]
                  in if null paths then [] else createVars paths xs []

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

createVars :: [Int] -> [Int] -> [[Int]] -> [[Int]]
createVars [] _ r = r
createVars (p:paths) xs res = createVars paths xs res ++ [p:xs] 

editN n xs ne = take n xs ++ [ne] ++ drop (n + 1) xs

-- Задача 8 -----------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort [] xs = null xs
isTopolSort gr xs  = and $ map (check) edges
                    where numerated = zip [0..length gr] gr
                          edges = concatMap (\(a, xs) -> arring a xs) numerated
                          check (a, b) = elem b right
                                         where (_, right) = saf a xs
                          

arring a [] = []
arring a (x:xs) = [(a, x)] ++ arring a xs

-- Задача 9 -----------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr a b = if null way then Nothing else Just $ reverse way
                where res = until (\(a, b) -> null a) (stepW2 b gr) ([[a]], []) 
                      way = snd res

stepW2 :: Int -> Graph -> ([[Int]], [Int]) -> ([[Int]], [Int])
stepW2 b gr (xs, ways) = (res, survive res ways)
                      where res = concatMap (appUtil2 gr) xs
                            maxL = length ways
                            survive r ys = if null mm then ys else head mm
                                           where mm = filter (\ss -> head ss  == b 
                                                              && length ss > maxL) r
appUtil2 :: Graph -> [Int] -> [[Int]]
appUtil2 gr (xs) = let paths = [ a | a <- (gr !! head xs), a `notElem` xs]
                  in if null paths then [] else createVars paths xs []


--- Задача 10 ----------------------------------------
merge :: [Int] -> [Int] -> [Int]
merge xs ys = myNub $ merge' xs ys

merge' :: [Int] -> [Int] -> [Int]
merge' [] xs = xs
merge' xs [] = xs
merge' (x:xs) (y:ys)
  | x < y = x:(merge' xs (y:ys))
  | y < x = y:(merge' (x:xs) ys)
  | otherwise = x:(merge' xs ys)

--- Задача 11 ----------------------------------------

charHex :: Int -> Char
charHex ch
    | ch == 0 = '0'
    | ch == 1 = '1'
    | ch == 2 = '2'
    | ch == 3 = '3'
    | ch == 4 = '4'
    | ch == 5 = '5'
    | ch == 6 = '6'
    | ch == 7 = '7'
    | ch == 8 = '8'
    | ch == 9 = '9'
    | ch ==  10 = 'a'
    | ch ==  11 = 'b'
    | ch ==  12 = 'c'
    | ch ==  13 = 'd'
    | ch ==  14 = 'e'
    | ch ==  15 = 'f'
    | otherwise  = '?'

intToString :: Int -> Int -> String
intToString 0 _ = "0"
intToString int lim = go int []
    where go :: Int -> String -> String
          go 0 str = str 
          go i str = (go left str) ++ [charHex remi]
                where remi = mod i lim
                      left = div i lim

--- Задача 12 ----------------------------------------
hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'a' = 10
    | ch == 'b' = 11
    | ch == 'c' = 12
    | ch == 'd' = 13
    | ch == 'e' = 14
    | ch == 'f' = 15
    | otherwise  = -1

stringToInt :: Int -> String -> Maybe Int
stringToInt lim str = res
                      where checker [] = True
                            checker (x:xs) = (hexChar x /= -1 && hexChar x < lim) && checker xs
                            convU []     = 0
                            convU (x:xs) = let decod = hexChar x in decod + lim * (convU xs)
                            res = case checker str of
                                      True -> Just $ convU (reverse str)
                                      False -> Nothing
                             

--- Задача 13 ----------------------------------------
genExpr :: Int -> Int -> [String]
genExpr i res = suits
                where str = show i
                      size = (length str) - 1
                      combs = combos size ['+','-','*']
                      f '+' = (+)
                      f '-' = (-)
                      f '*' = (*)
                      operate :: [Char] -> [Char] -> Int -> Int
                      operate [] _ x = x
                      operate _ []  x = x
                      operate (v1:vals) (o:ops) x = operate vals ops $ (f o) x (hexChar v1)
                      suits = [trimm str j | j <- combs, 
                              operate (tail str) j (hexChar $ head str) == res]
                      trimm s c = (head s):(insmath (tail s) c)
                      insmath [] [] = []
                      insmath (k:ks) (l:ls) = l:k:(insmath ks ls)



combos :: Int -> [a] -> [[a]]
combos 0 lst = [[]]
combos n lst = [x:xs | x <- lst, xs <- combos (n-1) lst]

--- Задача 14 ----------------------------------------
genExprBracket :: Int -> Int -> [String]
genExprBracket digs k = map show (solutions k [read [l] | l <- ll])
                        where ll = show digs


results :: [Int] -> [Result] 
results []  = [] 
results [n] = [(Val n, n) | n >= 0]
results ns  = [e | (ls,rs) <- splits ns,
                          l <- results ls,
                          r <- results rs,
                          e <- combine1 l r ]

combine1 :: Result -> Result -> [Result]
combine1 (l,x) (r,y) = [(App o l r, apply o x y) | o <- [Add,Sub,Mul]]

solutions :: Int -> [Int] -> [Expr]
solutions n ns = [e | (e,m) <- results ns, m == n]


apply :: Op -> Int -> Int -> Int
apply Add v1 v2 = v1 + v2 
apply Sub v1 v2 = v1 - v2
apply Mul v1 v2 = v1 * v2

values :: Expr -> [Int] 
values (Val n)     = [n] 
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0] 
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r] 

solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n =  elem (values e) [ns]&& eval e == [n]

splits  :: [a] -> [([a],[a])]
splits xs = [ splitAt i xs | i<-[1..length xs-1] ]

--- Задача 15 ----------------------------------------

isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 Null23 = True
isTree23 (Leaf _) = True
isTree23 tree@(Fork2 tl x tr ) = c1 && c2 && gtL && ltR && (last lc <= x) && (head rc == x)
                                 where c1 = height tl == height tr
                                       c2 = and $ map (notNullTree) [tl, tr] 
                                       lc = getCrone tl
                                       rc = getCrone tr
                                       gtL = null $ filter (>= x) lc
                                       ltR = null $ filter (<  x) rc


isTree23 tree@(Fork3 tl x tm y tr) = c1 && c11 && c2 && gtL && ltR && cM1 && cM2 && (x < y) 
                                      && (last lc <= x) && (head rc == y) && (head mc == x) && (last mc <= y)
                                     where c1 = height tl == height tr
                                           c11 = height tl == height tm
                                           c2 = and $ map (notNullTree) [tl, tm, tr] 
                                           lc = getCrone tl
                                           rc = getCrone tr
                                           mc = getCrone tm
                                           gtL = null $ filter (>= x) lc
                                           cM1 = null $ filter (<  x) mc
                                           cM2 = null $ filter (>= y) mc
                                           ltR = null $ filter (<  y) rc


notNullTree Null23 = False
notNullTree _ = True

height :: (Ord a) =>  Tree23 a -> Double
height (Leaf _) = 1
height (Fork2 tl _ tr ) = 1 + ((height tr) + (height tl)) / 2
height (Fork3 tl _ tm _ tr) = 1 + ((height tr) + (height tl) + (height tm)) / 3


getCrone :: (Ord a) => Tree23 a -> [a]
getCrone Null23 = []
getCrone (Leaf l) = [l]
getCrone (Fork2 tl _ tr ) = getCrone tl ++ (getCrone tr)
getCrone (Fork3 tl _ tm _ tr) = getCrone tl ++ (getCrone tm) ++ (getCrone tr)

--- Задача 16 ----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 =  getVals t1 [] ==  getVals t2 []

getVals :: (Ord a) => Tree23 a -> [a] -> [a]
getVals Null23 v = v 
getVals (Leaf k) v = k:v
getVals (Fork2 tl _ tr) v = (getVals tl v) ++ (getVals tr v) ++ v
getVals (Fork3 tl _ tm _ tr) v = (getVals tl v) ++ (getVals tm v) ++ (getVals tr v) ++ v


--- Задача 17 ----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23  tr e = elem e $ getVals tr []

--- Задача 18 ----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23  = undefined

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Fork2 (Leaf _) _ _)     = True 
isTerminal (Fork3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Fork2 або Fork3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insFork v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insFork v tr - додає значення v в дерево tr з корнем - нетермінальний вузол 
insFork :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insFork = undefined

---------------------Тестові дані 

---------------------- Графи -------
gr1, gr2, gr3:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]

---------------------- 2-3-дерева
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Fork2 (Fork2 (Fork2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Fork2 (Leaf 2) 3 (Leaf 3)))
              4
             (Fork2 (Fork2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Fork2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Fork3 (Fork2 (Leaf 0) 1 (Leaf 1))
              2
             (Fork3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Fork3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork2 (Leaf 16) 19 (Leaf 19))

tr4 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5))
            7
            (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Fork2 (Fork2 (Fork2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Fork2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Fork2 (Fork2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )