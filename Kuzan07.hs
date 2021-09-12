-- # OPTIONS_GHC -Wall #
module Kuzan07 where
import Data.List
data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-äåðåâî ïîðÿäêà t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- ãîëîâí³ õàðàêòåðèñòèêè B-äåðåâî  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

ascends xs = and $ zipWith (<=) xs $ drop 1 xs
adjacent = zip <*> tail
allTheSame xs = and $ map (== head xs) (tail xs)

getVal :: BinTreeM a -> Maybe (a, Int)
getVal EmptyM = Nothing
getVal (NodeM v k _ _ ) = Just (v, k)

getTrees :: BinTreeM a -> (BinTreeM a, BinTreeM a)
getTrees (NodeM _ _ lt rt) = (lt,rt)

getKeys :: Btree b -> [b]
getKeys (NodeB xs _) = xs

getKids :: Btree b -> [Btree b]
getKids (NodeB _ xs) = xs

-- Çàäà÷à 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch tree@(NodeM v k tl tr)  =  k > 0 && (checkLvl v (getTrees tree)) && (isSearch tl) && (isSearch tr)

checkLvl::(Ord a) => a -> (BinTreeM a, BinTreeM a) -> Bool
checkLvl a (tr1, tr2)  
      | (checkLeft == Just False) || (checkLeft == Just False) = False
      | otherwise = True
        where  left  = fmap fst (getVal tr1)
               right = fmap fst (getVal tr2) 
               checkLeft =  (Just (<= a)) <*> left
               checkRight = (Just (>= a)) <*> right

-- Çàäà÷à 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM v k tl tr) val = (v == val) || (elemSearch tl val) || (elemSearch tr val)

-- Çàäà÷à 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM val = NodeM val 1 EmptyM EmptyM
insSearch (NodeM v k tl tr) val
    | v == val = (NodeM v (k + 1) tl tr)
    | v < val = NodeM v k (insSearch tl val) tr
    | v > val = NodeM v k tl (insSearch tr val)

-- Çàäà÷à 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch EmptyM val = EmptyM
delSearch tree@(NodeM v k tl tr) val
    | v == val = if k >= 2 then (NodeM v (k - 1) tl tr)
                           else if (tl == EmptyM) then tr
                           else if (tr == EmptyM) then tl
                           else deleteHead tree

    | v < val = NodeM v k (delSearch tl val) tr
    | v > val = NodeM v k tl (delSearch tr val)

deleteHead :: (Ord a) => BinTreeM a -> BinTreeM a
deleteHead tree@(NodeM v k tl tr) = let vals = snd (findMin (tree, (v, k)))
                                        nTrees = getTrees $ fst (findMin (tree, (v, k)))
                                    in NodeM (fst vals) (snd vals) (fst nTrees) (snd nTrees)

findMin ::(Ord a) => (BinTreeM a, (a, Int)) -> (BinTreeM a, (a, Int))
findMin (bt@(NodeM v k tl tr), (m, n))
 |tl /= EmptyM = (NodeM v k (fst $ findMin (tl, (m, n))) tr, snd $ findMin (tl, (m, n)))
 |tr /= EmptyM = (NodeM v k tl (fst $ findMin (tr, (m, n))), snd $ findMin (tr, (m, n)))
 | otherwise = (EmptyM, (v, k))

-- Çàäà÷à 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = trav (createTree xs)

trav :: (Ord a) => BinTreeM a -> [a]
trav EmptyM = []
trav tree@(NodeM v k tl tr) = (trav tr) ++ [v] ++ (trav tl)

createTree :: (Ord a) => [a] -> BinTreeM a
createTree xs = foldl (insDupl) EmptyM xs

insDupl :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insDupl EmptyM val = NodeM val 1 EmptyM EmptyM
insDupl (NodeM v k tl tr) val
    | v <= val = NodeM v k (insDupl tl val) tr
    | v >  val = NodeM v k tl (insDupl tr val)


-- Çàäà÷à 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a -> BInform a
-- findBInform Btree [] []
findBInform tr = let minV = minimum $ getKeys tr
                     maxV = maximum $ getKeys tr
                 in fst (iterInfo (BInform 0 minV maxV, (getKeys tr, getKids tr) ) )

iterInfo :: (Bounded a, Ord a) =>  (BInform a, ([a], [Btree a])) -> (BInform a, ([a], [Btree a]))
iterInfo (info@(BInform depth minV maxV), (vals, kids)) = 
  if null $ kids then (info, (vals, kids))
  else let newKids = concatMap (getKids) kids
           newVals = concatMap (getKeys) kids
           newExtremes = gatherInfo info newVals
           newMin = fst $ newExtremes
           newMax = snd $ newExtremes
       in iterInfo ((BInform (depth + 1) newMin newMax), (newVals, newKids))
  

gatherInfo :: (Bounded a, Ord a) => BInform a -> [a] -> (a, a)
gatherInfo (BInform _ minV maxV) xs = let highest = maximum xs
                                          lowest = minimum xs
                                      in ((min minV lowest), (max maxV highest))
-- Çàäà÷à 7 ------------------------------------
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
-- isBtree _ NodeB [] [] = False
isBtree p tree@(NodeB val kids) = let lv = length val
                                      lk = length kids
                                  in lv >= 1 && lv <= (2*p - 1) && lk == (lv + 1) && ascends val && checkKids p val kids

checkKids :: (Bounded a, Ord a) => Int -> [a] -> [Btree a] -> Bool 
checkKids p [] [] = True
checkKids p val kids =  let  anyl  = concatMap (checkNonRoot p) kids
                             vet   = map (fst) anyl
                             roles = map (snd) anyl
                             res1  = all (==True) vet
                             res2  = allTheSame roles
                             res3  = all(==True) $ concatMap (\(a, b, c) -> [a <= b && b <= c]) 
                                                 $ zipWith (\val (a, c) -> (a, val, c)) val 
                                                 $ concatMap (\(l, r) -> [(maximum l, minimum r)]) 
                                                 $ adjacent $ concatMap (\tr -> [getKeys tr]) kids
                             res   = res1 && res2 && res3
                        in res && if head roles == False then all (== True)
                                                         $ concatMap (\x -> [checkKids p (getKeys x) (getKids x)]) kids 
                                                         else True

checkNonRoot :: (Bounded a, Ord a) => Int -> Btree a -> [(Bool, Bool)]
checkNonRoot p tree@(NodeB val kids) = let lv = length val
                                           lk = length kids
                                           isOk = lv >= (p - 1) && lv <= (2*p - 1) && (lk == (lv + 1) || lk == 0) && ascends val
                                       in [(isOk, lk == 0)]

-- Çàäà÷à 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree _ tr1 tr2 = sort (unfold tr1) == sort (unfold tr2)

unfold (NodeB keys kids) = keys ++ concatMap (unfold) kids

-- Çàäà÷à 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
-- elemBtree (NodeB [] _) _ = False
elemBtree root@(NodeB keys kids) k = k `elem` unfold root

-- Çàäà÷à 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t tree@(NodeB keys kids) el = if isFull t tree
                                       then let res = splitNode tree
                                                key = fst res
                                                trs = snd res
                                            in insBtree t (NodeB [key] trs) el
                                       else if isLeaf tree then (NodeB (insertKey el keys) [])
                                                           else insertUtil t tree el
insertUtil :: Ord a => Int -> Btree a -> a -> Btree a
insertUtil t tree@(NodeB keys kids) el = let  index = (position el keys) 
                                              chosen = kids !! index
                                              res = splitNode chosen
                                              key = fst res
                                              trs = snd res
                                              oldTuple = splitAt index kids
                                              newKids = (fst oldTuple) ++ trs ++ (tail $ snd oldTuple)
                                              descend = (fst oldTuple) ++ [(insertUtil t chosen el)] ++ (tail $ snd oldTuple)
                                         in if isFull t chosen 
                                            then insertUtil t (NodeB (insertKey key keys) newKids) el
                                            else if isLeaf chosen then (NodeB keys $ (fst oldTuple) ++ [(NodeB (insertKey el (getKeys chosen)) [])]
                                                                                                    ++ (tail $ snd oldTuple))
                                                                  else (NodeB keys descend)

isFull :: Ord a => Int -> Btree a -> Bool
isFull p tr = length (getKeys tr) >= (2*p - 1)

isLeaf :: Ord a => Btree a -> Bool
isLeaf = null . getKids

insertKey :: Ord a => a -> [a] -> [a]
insertKey x xs = sort $ x:xs

position :: Ord a => a -> [a] -> Int
position t xs = length xs - length [x | x <- xs, x >= t]

decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB = undefined

splitNode :: Ord a => Btree a -> (a, [Btree a])
splitNode (NodeB keys kids) = let pos   = div (length keys) 2
                                  tupl  = splitAt pos keys
                                  left  = fst tupl
                                  mkey  = head $ snd tupl
                                  right = tail $ snd tupl
                              in if null kids then (mkey, [(NodeB left []), (NodeB right [])])
                                             else (mkey, ([(NodeB left $ take (pos + 1) kids), (NodeB right $ drop (pos + 1) kids)]))

---------------------Òåñòîâ³ äàí³ - Äåðåâà ïîøóêó -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   
bm1 :: BinTreeM Char
bm1 = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 0 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   


tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
