module Kuzan09 where

import Data.List
import Data.Char

import qualified Text.ParserCombinators.Parsec as P

data RE = Null      |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])
type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)
type MetaPack = ([MetaState], [MetaState], [MetaTransition]) 

-- Задача 1 -----------------------------------------
simplify :: RE -> RE
simplify Null = Null
simplify (Opt a) = (Alt (simplify a) Null)
simplify (Plus a) = (Seq (simplify a) $ Rep (simplify a)) 
simplify (Alt a1 a2) = (Alt (simplify a1) (simplify a2))
simplify (Rep a) = Rep $ simplify a
simplify (Seq e1 e2) = (Seq (simplify e1) (simplify e2))
simplify (Term c) = Term c

-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_, fins, _) i = i `elem` fins

isEssential :: Automation -> State -> Bool 
isEssential aut@(_, _, ways) i = isTerminal aut i || (not $ null $ filter (\(s,_,c) -> s == i && c /= Eps) ways)

-- -- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_, _, ways) i = filter (\(s,_,_) -> s == i) ways

-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels ts = nub $ filter (/= Eps) $ map (\(_,_,l) -> l) ts

-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA aut str = accepts aut str

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]
closOne :: Automation -> [State] -> [State] -> [State]

stStep aut@(_, _, ways) i l = map (\(_,s,_) -> s) $ filter (\(start, _, lbl) -> start == i && lbl == l) ways
setStep aut bs mc = nub $ concatMap (\x -> stStep aut x mc) bs

closure aut ss = sort $ nub $ ss ++ closOne aut ss []
closOne aut [] found = found
closOne aut starts found = let fetched = filter (\x -> x `notElem` found) $ setStep aut starts Eps
                           in closOne aut fetched $ found ++ fetched

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts aut@(i,_,_) str = find (isTerminal aut) rolled /= Nothing 
                          where rolled = accUtil aut (closure aut [i]) str
                                accUtil aut ss [] = ss
                                accUtil aut ss (x:xs) = let vars = closure aut $ setStep aut ss $ C x in accUtil aut vars xs

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null beg fin nxt = ([(beg, fin, Eps)], nxt)
make (Term t) beg fin nxt = ([(beg, fin, C t)], nxt)
make (Seq a b) beg fin nxt = ((nxt, nxt + 1, Eps) : r1 ++ r2, s2)
                              where (r1, s1) = make a beg nxt (nxt + 2)
                                    (r2, s2) = make b (nxt + 1) fin s1
make (Rep a) beg fin nxt = ((beg, nxt, Eps) : (beg, fin, Eps) : (nxt + 1, fin, Eps) : (nxt + 1, nxt, Eps) : t1, s1)
                           where (t1, s1) = make a nxt (nxt + 1) (nxt + 2)
make (Alt a b) beg fin nxt =  ((beg, nxt, Eps) : (nxt + 1, fin, Eps) :
                              (beg, nxt + 2, Eps) : (nxt + 3, fin, Eps) : t1 ++ t2, s2)
                              where (t1, s1) = make a nxt (nxt + 1) (nxt + 4)
                                    (t2, s2) = make b (nxt + 2) (nxt + 3) s1


-- Задача 9 -----------------------------------------
parseReg :: String -> Maybe RE 
parseReg s = let res = P.parse preg "" (filter (not . isSpace) s) in case res of 
  Right a  -> Just a
  Left _   -> Nothing

preg :: P.Parser RE
preg = do r <- rexpr
          P.eof
          return r

altList :: P.Parser [RE]
altList = P.many $ do
            P.char '|'
            r <- rterm
            return r

rexpr :: P.Parser RE
rexpr = do re <- rterm
           lst <- altList
           if null lst then return re else return $ Alt re $ recAlt lst

rterm :: P.Parser RE
rterm = do re <- P.many1 rfact
           return $ recSeq re

recElse :: RE -> String -> RE
recElse r [] = r
recElse r (x:xs)
    | x == '*' = Rep $ recElse r xs
    | x == '+' = Plus $ recElse r xs
    | otherwise = Opt $ recElse r xs

recAlt :: [RE] -> RE
recAlt [x] = x
recAlt (x:xs) = Alt x (recAlt xs)

recSeq :: [RE] -> RE
recSeq [x] = x
recSeq (x:xs) = Seq x (recSeq xs)

prime :: P.Parser RE
prime = (P.try rsymb) P.<|> curled

rfact :: P.Parser RE
rfact = do pr <- prime
           op <- P.many $ P.oneOf "*?+"
           return $ recElse pr $ reverse op

curled :: P.Parser RE
curled = do P.char '('
            r <- rexpr
            P.char ')'
            return r

rsymb :: P.Parser RE
rsymb = do t <- P.noneOf "()|+*?"
           return (Term t) 


-- Задача 10 -----------------------------------------
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' aut@(i,_,_) = let isx = filter (isEssential aut) (closure aut [i])
                          init = ([], [isx], [])          
                          tupl = until (\(_, a, _) -> null a) (maker aut) init                
                          cut = (\(a, _, c) -> (head a, a, c))
                      in  cut tupl

maker :: Automation -> MetaPack -> MetaPack
maker aut (gmsx, bmsx, mtrx) = insrt newBase msx mlxs lbls
                            where msx = head bmsx
                                  lbls = nub $ concatMap (\x -> labels $ transitionsFrom aut x) msx
                                  wideml = map (\x -> closure aut $ setStep aut msx x) lbls
                                  mlxs = map (filter $ isEssential aut) wideml
                                  newBase = (gmsx ++ [msx], tail bmsx, mtrx) 

insrt :: MetaPack -> MetaState -> [MetaState] -> [Label] -> MetaPack
insrt base _ [] _ = base
insrt (gmsx, bmsx, mtrx) msx mlx lbls = insrt (gmsx, bmsx ++ mbNew, mtrx ++ newWay) msx (tail mlx) (tail lbls)
                                         where cand = head mlx
                                               mbNew = if elem cand bmsx || elem cand gmsx then [] else [cand]
                                               newWay = [(msx, cand, head lbls)]

makeDA :: Automation -> Automation
makeDA (f, fins, ways) = (1, newFins, sort newWays)
     where (_, metaSts, metaWays) = makeDA' (f, fins, ways)
           enumerated = zip metaSts [1..]            
           nef m  =  find (\x -> elem x fins) m /= Nothing
           newFins = map snd $ filter (\(a, b) -> nef a) enumerated 
           newWays = [(reid metaSts from, reid metaSts to, c) | (from, to, c) <- metaWays]
           reid xs i = case (elemIndex i xs) of
                        Just a -> a + 1

-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
