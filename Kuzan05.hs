module Kuzan05 where

import Data.Char(isUpper)
import Data.List


type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

ifLL1 :: Grammar -> Bool
ifLL1 gr  = let fst = buildFst gr
                nxt = buildNxt gr fst
            in testingLL1 gr fst nxt

justNxt :: Grammar -> Predict
justNxt gr = let fst = buildFst gr
             in buildNxt gr fst
            

gr10 = [('S', "abAS"),('S', "b"),('A', "bSA"), ('A', "")]
gr11 = [('S',"cBcA"),('A', ""),('A', "BS"),('A', "bA"), ('B', "bB"), ('B', "")]

-- Задача 1 ------------------------------------
addUtil::String -> Char -> Maybe Char
addUtil st c = if length [x | x <- st, x == c] == 0 then Just c else Nothing

addOne :: String -> Char -> String  
addOne st c = case addUtil st c of
    Just c -> sort(st ++ [c])
    Nothing -> sort st

addAll :: String -> String -> String 
addAll org new = sort(org ++ [ x | x <- new, addUtil org x /= Nothing])

addWithout :: String -> String -> String 
addWithout org new = sort (org ++ [ x | x <- new, addUtil org x /= Nothing, 
                                  addUtil org x /= Just '$'])

inter :: String -> String -> String 
inter org new = nub $ sort [ x | x <- new, addUtil org x == Nothing] 

-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String 
tkPredict pt c =  let llist = [x | x <- pt, fst x == c] in 
                             if null llist then "" else snd (head llist)

insertAlp :: Predict -> Char -> String -> Int 
insertAlp [] c st = 0
insertAlp (p:pt) c st = if c > fst p then length pt else insertAlp pt c st

insertUtil :: Predict -> Char -> String -> Predict
insertUtil p c st = let i = length p - insertAlp p c st in 
                            take i p ++ [(c, st)] ++ drop i p

upPredict :: Predict -> Char -> String -> Predict 
upPredict pt c st = if tkPredict pt c == ""
                       then insertUtil pt c st
                       else [if fst x == c then (c, st) else x | x <- pt]

-- Задача 3 ------------------------------------

getLst :: (String, String, Maybe [Int]) -> Maybe [Int]
getLst (x, y, z) = z

parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gr ctl str = getLst (until (check) (step gr ctl) (str, [fst $ head gr], Just []))

check :: (String, String, Maybe [Int]) -> Bool
check (x, y, z) = (x == "" && y == "") || z == Nothing

step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step _ _ ([],_,_) = ("","",Nothing)
step _ _ (_,[],_) = ("","",Nothing)
step gr ctl (as@(a:axs), bs@(b:bxs), c)  = if a == b
                             then (axs, bxs, c)
                             else if isUpper b
                                then let id = findIndex (\(j, k) -> j == b && head k == a) gr
                                   in case id of 
                                     Nothing -> (as, bs, Nothing)
                                     Just v  -> (as, snd (gr !! v) ++ bxs,
                                        case c of
                                           Nothing -> Just [v] 
                                           Just t -> Just (t ++ [v]))
                               else (as, bs, Nothing)


-- Задача 4 ------------------------------------
first :: Predict -> String -> String
first _ "" = "$" 
first pt xs = sort $ firstUtil pt xs ""

firstUtil :: Predict -> String -> String -> String
firstUtil _ [] _ = [] 
firstUtil pt (x:xs) res = if isUpper x then
                       let symb = snd $ head $ [a | a <- pt, fst a == x] in
                         if '$' `elem` symb then 
                            if length xs /= 0 then 
                            firstUtil pt xs (addWithout res symb)
                            else addAll res symb
                          else addAll res symb
                       else res++[x]

-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl gr pfst pnxt = nub $ sort $ concatMap (controlUtil gr pfst pnxt) $ ccc gr

ccc :: [Production] -> [(Int, Production)]
ccc xs = zip [0..] xs

controlUtil :: Grammar -> Predict -> Predict -> (Int, Production) -> Control
controlUtil g pfst pnxt (i,pr) = let firstRes = first pfst $ snd pr in
                                if '$' `elem` firstRes
                                    then let followRes = snd $ head [x | x <- pnxt, (fst x) == (fst pr)]
                                    in (createCtrl firstRes (fst pr) i) ++ (createCtrl followRes (fst pr) i)
                                else createCtrl firstRes (fst pr) i

createCtrl :: [Char] -> Char -> Int -> Control
createCtrl str c i  = concatMap (createC c i) str

createC :: Char -> Int -> Char -> [((Char, Char), Int)]
createC x i c = [((x,c), i)]

-- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 gr pfst pnxt = not $ False `elem` (concatMap (anyl pfst pnxt) (fromGrammar gr))

anyl :: Predict -> Predict -> (Char, [String]) -> [Bool]
anyl pfst pnxt tupl = let aCond = testFst pfst $ snd tupl
                          bCond = testFollow pfst pnxt (fst tupl)
                      in [if "$" `elem` snd tupl then  aCond && bCond else aCond]

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar xs = concatMap (getRights) (groupBy (\a b -> fst a == fst b) $ sort xs)

getRights :: [(Char, String)] -> [(Char, [String])]
getRights xs = [(fst $ head xs, map snd xs)]

testFst :: Predict -> [String] -> Bool
testFst pt strings = let pairs = filter ((2==).length) $ subsequences (map (first pt) strings) 
                     in null [ x | x <- pairs, not $ null $ intersect (x !! 0) (x !! 1)]

testFollow :: Predict -> Predict -> Char -> Bool
testFollow pfst pnxt c = null $ intersect  (snd $ head [ x | x <- pfst, fst x == c])
                                           (snd $ head [ x | x <- pnxt, fst x == c])

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict 
buildFst gr = let pair = formatGr gr ([], createTbl gr)
                  newGr = fst pair
                  startTbl = snd pair
              in snd $ until (checker) (stepF newGr) ([], startTbl)

checker :: (Predict, Predict) -> Bool
checker pt = fst pt == snd pt

createTbl :: Grammar -> Predict
createTbl gr = let lst = nub $ map fst $ fromGrammar gr 
               in [(x, "") | x <- lst]

formatGr :: Grammar -> (Grammar, Predict) -> (Grammar, Predict) 
formatGr [] c = c 
formatGr (g:gr) (ntgr, pt)  = if isUpper (if snd g /= "" then head (snd g) else '$') 
                              then formatGr gr (ntgr ++ [g], pt)
                              else formatGr gr (ntgr, apply g pt)

apply :: Production -> Predict -> Predict
apply rule pt = if (snd rule) == ""
                then [if fst x == fst rule then (fst rule, sort $ nub $ snd x ++ "$") else x | x <- pt]
                else [if fst x == fst rule then (fst rule, sort $ nub $ snd x ++ [head $ snd rule]) else x | x <- pt]


stepF :: Grammar -> (Predict, Predict) -> (Predict, Predict)
stepF gr (ptOld, pt) = (pt, stepF' gr pt)  

stepF' :: Grammar -> Predict -> Predict
stepF' [] pt = pt
stepF' (g:gr) pt = stepF' gr (secondRule g (firstRule g pt))

firstRule :: Production -> Predict -> Predict
firstRule (c, str) pt = if null [not $ isUpper x | x <- str] && (checkNullable str pt)
                        then [if fst x == c then (fst x, sort $ snd x ++ ['$']) else x | x <- pt]
                        else pt

checkNullable :: String -> Predict -> Bool
checkNullable []  pt = True
checkNullable "$" pt = True
checkNullable (x:xs) pt = if '$' `elem` (snd (head [ a | a <- pt, fst a == x ]))
                          then checkNullable xs pt
                          else False

secondRule :: Production -> Predict -> Predict
secondRule (c, []) pt = pt
secondRule (c, str) pt = let sub = snd (head [ x | x <- pt, fst x == head str])
                             newpt = [if fst x == c then (c, sort $ nub $ addWithout (snd x) sub) else x | x <- pt]
                         in          if '$' `elem` snd (head [ x | x <- pt, fst x == c]) && (length str) > 1
                                     then secondRule (c, tail str) newpt
                                     else newpt
                             
 
-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict
buildNxt gr pfst = let pair = prepare gr pfst
                       newGr = fst pair
                       startTbl = snd pair
                   in snd $ until (checker) (iteration' newGr) ([], startTbl)

prepare :: Grammar -> Predict -> (Grammar, Predict)
prepare gr pfst = pointStart gr (formatGN gr pfst ([], createTbl gr))

formatGN :: Grammar -> Predict -> (Grammar, Predict) -> (Grammar, Predict) 
formatGN [] _ c = c 
formatGN (g:gr) pfst (ntgr, pt)  = if (null $ filter (isUpper) (snd g)) || snd g == ""
                                   then formatGN gr pfst (ntgr, pt)
                                   else if (isUpper (last (snd g))) && last (snd g) /= fst g
                                        then formatGN gr pfst (applyN (fst g, dissect (snd g)) pfst (ntgr ++ [(fst g, [last (dissect (snd g))]++['$'])], pt))
                                        else formatGN gr pfst (applyN (fst g, dissect (snd g)) pfst (ntgr, pt))

pointStart :: Grammar -> (Grammar, Predict) -> (Grammar, Predict) 
pointStart gr0 (gr, pt) = (gr, [if fst a == fst (head gr0) then (fst a, sort $ snd a ++ "$") else a | a <- pt])

applyN :: Production -> Predict -> (Grammar, Predict) -> (Grammar, Predict)
applyN (c, (x:[])) _    (gr, pt) = (gr, pt)
applyN (c, (x:xs)) pfst (gr, pt) = if isUpper x
                                   then let sub = (first pfst xs) in
                                          if '$' `elem` sub
                                          then applyN (c, xs) pfst ((gr ++ [(c, [x]++[x])]), [if fst a == x then (x, addWithout (snd a) sub) else a | a <- pt])
                                          else applyN (c, xs) pfst (gr, [if fst a == x then (x, addAll     (snd a) sub) else a | a <- pt])
                                   else applyN (c, xs) pfst (gr, pt)
 
iteration' :: Grammar -> (Grammar, Predict) -> (Grammar, Predict) 
iteration' gr (ptOld, pt) = (pt, iteration gr pt)

iteration :: Grammar -> Predict -> Predict
iteration []     pt = pt
iteration (g:gr) pt = let lhs = fst g
                          rhsAll = snd g
                          rhs = head $ rhsAll
                      in if length rhsAll == 1
                         then iteration gr (insertNxt (getNxt pt rhs) lhs (insertNxt (getNxt pt lhs) rhs pt))
                         else iteration gr (insertNxt (getNxt pt lhs) rhs pt)


insertNxt :: String -> Char -> Predict -> Predict
insertNxt sub c pt = [if fst x == c then (c, sort $ nub $ addAll (snd x) sub) else x | x <- pt]

getNxt :: Predict -> Char -> String
getNxt pt c = snd $ head $ filter (\(a, b) -> a == c) pt

dis :: String -> String
dis = fst . fmap (drop 1) . break (isUpper)

dissect :: String -> String
dissect str = str \\ (dis str)

leaveNT :: Production -> Production
leaveNT (c, str) = (c, dissect str)

nontermTails :: Grammar -> [(Char, String)] 
nontermTails = undefined

evalNxt :: [(Char,String)] -> Predict -> Predict -> Predict
evalNxt = undefined

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne = undefined

---------------------Тестові дані ---------------------------
myTestLL1 :: [Bool]
myTestLL1 = [testingLL1 gr0 pFst0 pNxt0, testingLL1 gr1 pFst1 pNxt1, testingLL1 gr2 pFst2 pNxt2, testingLL1 gr3 pFst3 pNxt3, testingLL1 gr4 pFst4 pNxt4, testingLL1 gr5 pFst5 pNxt5]
myTestFst = [buildFst gr0 == pFst0, buildFst gr1== pFst1, buildFst gr2 == pFst2, buildFst gr3 == pFst3,buildFst gr4 == pFst4, buildFst gr5 == pFst5]
myTestNxt = [buildNxt gr0 pFst0 == pNxt0, buildNxt gr1 pFst1 == pNxt1, buildNxt gr2 pFst2 == pNxt2, buildNxt gr3 pFst3 == pNxt3, buildNxt gr4 pFst4 == pNxt4, buildNxt gr5 pFst5 == pNxt5] 
myTestCtrl = [buildingControl gr0 pFst0 pNxt0 == ctl0, buildingControl gr1 pFst1 pNxt1 == ctl1, buildingControl gr2 pFst2 pNxt2 == ctl2]

gr0, gr1, gr2, gr3, gr4, gr5 :: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]

