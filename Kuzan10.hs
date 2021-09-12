module Kuzan10 where
import Data.List

-- розглядаємо лише цілі дані: скаляри  і масиви  
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))

-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b [] = [(a, b)]
updateValue a b (x:xs)
 | fst x == a    = (a, b) : xs
 | otherwise = x : updateValue a b xs

-- Задача 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A xs) (I a) (I b) = A (updateValue a b xs)
updateArray _ _ _ = error "Wronf input"

-- Задача 3 ------------------------------------
applyOp :: Op -> Value -> Value -> Value 
applyOp Add (I a) (I b) = I (a + b)
applyOp Minus (I a) (I b) = I (a - b)
applyOp Mul (I a) (I b) = I (a * b)
applyOp Less (I a) (I b)
 | a < b = I 1
 | otherwise = I 0
applyOp Equal a b 
 | a == b = I 1
 | otherwise = I 0
applyOp Index (A []) (I _) = I 0
applyOp Index (A ((a, b) : xs)) (I i)
 | a == i = I b
 | otherwise = applyOp Index (A xs) (I i)
applyOp _ _ _ = error "Wrong input"
 

-- Задача 4 ------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value 
evExp (Const i) _ _ = I i
evExp (Var v) _ st = lookUp v st
evExp (OpApp op a b) f st = applyOp op (evExp a f st) (evExp b f st)
evExp (Cond c tr fl) f st = if evExp c f st == I 0 then evExp fl f st else evExp tr f st
evExp (FunApp i es) fs st = evExp ex fs state
                          where (vardefs, ex) = lookUp i fs
                                args = evArgs es fs st
                                state = zip (map unpack vardefs) args
                                

evArgs :: [Exp] -> [FunDef] -> StateP -> [Value]
evArgs es fs st = map (\f -> evExp f fs st) es

unpack x = case x of
  Arr h -> h
  Int h -> h

uu x = case x of
  Just k -> k

-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign i ex) fs ps st = updateValue i (evExp ex fs st) st

evStmt (AssignA i ex1 ex2) fs ps st = updateValue i ar1 st
                                      where ar = lookUp i st     
                                            p1 = evExp ex1 fs st                                 
                                            p2 = evExp ex2 fs st                                 
                                            ar1 = updateArray ar p1 p2

evStmt (If c o1 o2) fs pds st = let I i = evExp c fs st in evStmt (if i /= 0 then o1 else o2) fs pds st

evStmt stmt@(While c stm) fs pds st = if i == 0 then st else res
                                    where I i = evExp c fs st
                                          nst = evStmt stm  fs pds st
                                          res = evStmt stmt fs pds nst

evStmt (Block vs stms) fs dps st = drop (length vs) res 
                                    where expand = (map initv vs) ++ st
                                          res = foldl evalu expand stms
                                          evalu = \a b -> evStmt b fs dps a

evStmt (Call i es) fs ps st = drop (length vals) $ evStmt stmt fs ps base
                              where (def, stmt) = lookUp i ps
                                    vals = evArgs es fs st
                                    base = (zip (map unpack def) vals) ++ st

-- Задача 6 ------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type   
iswfExp (Const _) _ _ = Just It
iswfExp (Var i) vs _ = lookup i vs
iswfExp (OpApp op e1 e2) vs fs = lf >> rt >> iswfOp op [uu lf, uu rt]
                                 where lf = iswfExp e1 vs fs
                                       rt = iswfExp e2 vs fs

iswfExp (Cond c l r) vs fs = mc >> ml >> mr >> iswfCond [uu mc, uu ml, uu mr]
                                 where mc = iswfExp c vs fs 
                                       ml = iswfExp l vs fs
                                       mr = iswfExp r vs fs                                       

iswfExp (FunApp i es) vs fs = f >> if xx == map Just (uu f) then Just It else Nothing
                               where f = lookup i fs
                                     xx = map (\e -> iswfExp e vs fs) es                                     

-- Задача 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign i e) ve fe _ = x /= Nothing && y /= Nothing && uu x == uu y  
                                   where x = lookup i ve 
                                         y = iswfExp e ve fe
  
iswfStmt (AssignA i e1 e2) ve fe _ = if uf /= Nothing then p else False
                                     where uf = a >> b >> c 
                                           a = lookup i ve
                                           b = iswfExp e1 ve fe
                                           c = iswfExp e2 ve fe
                                           p = iswfAssignA [uu a, uu b, uu c]                                          

iswfStmt (If c o1 o2) ve fe pe = Just It == mc && mo1 && mo2
                                 where mc = iswfExp c ve fe
                                       mo1 = iswfStmt o1 ve fe pe
                                       mo2 = iswfStmt o2 ve fe pe

iswfStmt (While c stmt) ve fe pe = c1 && c2
                                    where c1 = iswfExp c ve fe == Just It
                                          c2 = iswfStmt stmt ve fe pe

iswfStmt (Call i es) ve fe pe = a /= Nothing && map Just (uu a) == b
                                where a = lookup i pe
                                      b = map (\x -> iswfExp x ve fe) es

iswfStmt (Block vs stms) ve fe pe = and bb
                                    where xx = map initv' vs ++ ve
                                          bb = map (\x -> iswfStmt x xx fe pe) stms

initv' :: VarDef -> (Id, Type)
initv' (Arr v) = (v, At)
initv' (Int v) = (v, It)


-- Задача 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (i, (defs, e)) fe = if cc then check else False
                               where ll = lookup i fe
                                     xx = iswfExp e (map initv' defs) fe
                                     cc = (ll >> xx) /= Nothing
                                     check = map (\x -> snd $ initv' x) defs == (uu ll)
                                     

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (_, (defs, stmt)) ve fe pe = iswfStmt stmt (map initv' defs ++ ve) fe pe

-- Задача 9 ------------------------------------
iswfProgram :: Program -> Bool
iswfProgram (vd, fd, pd) = unq && ftest && ptest && main2
                           where unq1 = isUniq vd
                                 unq2 = checkUniq fd
                                 unq3 = checkUniq fd
                                 main1 = lookup "main" pd
                                 state = map initv' vd
                                 fenv = map createEnv fd
                                 penv = map createEnv pd
                                 ftest = and $ map (\x -> iswfFunDef x fenv) fd
                                 ptest = and $ map (\x -> iswfProcDef x state fenv penv) pd
                                 unq  = main1 /= Nothing && unq1 && unq2 && unq3
                                 main2 = iswfProcDef ("", (uu main1)) state fenv penv


checkUniq :: [(String, a)] -> Bool
checkUniq []     = True
checkUniq (x:xs) = fst x `notElem` (map fst xs) && checkUniq xs

isUniq :: (Eq a) => [a] -> Bool
isUniq xs = length (nub xs) == length xs

createEnv (i, (vd, _)) = (i, map (\x -> snd $ initv' x) vd)


--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Реалізація виконання програми 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - перевіряє коректність типів операндів ts 
--     бінарної операції o і формує тип результату Just t або Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
