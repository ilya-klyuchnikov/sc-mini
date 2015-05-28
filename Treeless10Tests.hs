module Treeless10Tests where

data GName = Pred | Zero deriving (Show, Eq)
data Exp = GVar1 | GCall1 GName Exp | Ctr Ctr deriving (Show, Eq)
data Ctr = Ctr0 String | Ctr1 String Exp | Ctr2 String Exp Exp deriving (Show, Eq)

data Fun = GFun1 GName String Exp
type Program = [Fun]

isGVar1 GVar1   = True
isGVar1 (Ctr c) = False

isGFun :: Fun -> Bool 
isGFun (GFun1 _ _ _) = True

and1 False _ = False
and1 True x = x

cnEq cn1 cn2 = cn1 == cn2
gnEq gn1 gn2 = gn1 == gn2

eval :: Exp -> Program -> Exp        
eval (Ctr c) p         = Ctr (evalCtr c p)
eval (GCall1 gn ctr) p = evalGCall1 ctr p gn 

evalCtr :: Ctr -> Program -> Ctr
evalCtr (Ctr0 s) p       = Ctr0 s
evalCtr (Ctr1 s e) p     = Ctr1 s (eval e p)
evalCtr (Ctr2 s e1 e2) p = Ctr2 s (eval e1 p) (eval e2 p)

eval01 :: Exp -> Program -> Exp -> Exp
eval01 GVar1 p gv1 = gv1
eval01 (Ctr c) p gv1 = Ctr (evalCtr01 c p gv1)
eval01 (GCall1 n arg) p gv1 = eval01GCall1 (isGVar1 arg) n arg p gv1

eval01GCall1 True  n arg p gv1 = evalGCall1 gv1 p n
eval01GCall1 False n arg p gv1 = evalGCall1 arg p n

evalCtr01 :: Ctr -> Program -> Exp -> Ctr
evalCtr01 (Ctr0 s) p fv1 = Ctr0 s
evalCtr01 (Ctr1 s e) p fv1 = Ctr1 s (eval01 e p fv1)
evalCtr01 (Ctr2 s e1 e2) p fv1 = Ctr2 s (eval01 e1 p fv1) (eval01 e2 p fv1)

evalGCall1 :: Exp -> Program -> GName -> Exp
evalGCall1 (Ctr ctr) p gn = evalGCall1Ctr ctr p gn

evalGCall1Ctr :: Ctr -> Program -> GName -> Exp
evalGCall1Ctr (Ctr1 cn arg1) p gn = evalGCall1a p p gn cn arg1

evalGCall1a :: Program -> Program -> GName -> String -> Exp -> Exp
evalGCall1a (fun:p1) p gn cn arg1 = evalGCall1b (isGFun fun) fun p1 p gn cn arg1 -- TODO

evalGCall1b :: Bool -> Fun -> Program -> Program -> GName -> String -> Exp -> Exp
evalGCall1b True  fun p1 p gn cn arg1 = evalGCall1с fun p1 p gn cn arg1
evalGCall1b False fun p1 p gn cn arg1 = evalGCall1a p1 p gn cn arg1

evalGCall1с :: Fun -> Program -> Program -> GName -> String -> Exp -> Exp
evalGCall1с (GFun1 gn' cn' e) p1 p gn cn arg1 = evalGCall1d (and1 (gnEq gn' gn) (cnEq cn' cn)) p1 p gn cn arg1 e
evalGCall1d False p1 p gn cn arg1 e = evalGCall1a p1 p gn cn arg1
evalGCall1d True  p1 p gn cn arg1 e = eval01 e p arg1


-------- examples ---------

data Unit = U
data Nat = S Nat | Z Unit

pred (S g1) = g1
pred (Z g1) = (Z g1)
zero (S g1) = zero g1
zero (Z g1) = (Z g1)

predProg :: Program
predProg = [
    GFun1 Pred "S" GVar1,
    GFun1 Pred "Z" (ctr "Z" [GVar1]),
    GFun1 Zero "S" (GCall1 Zero (GVar1)),
    GFun1 Zero "Z" (ctr "Z" [GVar1])
    ]

ctr :: String -> [Exp] -> Exp
ctr s [] = Ctr (Ctr0 s)
ctr s [e1] = Ctr (Ctr1 s e1)
ctr s [e1, e2] = Ctr (Ctr2 s e1 e2)

in2 :: Exp
in2 = GCall1 Pred (ctr "Z" [ctr "Unit" []])
result2 = eval in2 predProg
test2 = result2 == Ctr (Ctr1 "Z" (Ctr (Ctr0 "Unit")))

in3 :: Exp
in3 = GCall1 Pred (ctr "S" [(ctr "Z" [ctr "Unit" []])])
result3 = eval in3 predProg
test3 = result3 == Ctr (Ctr1 "Z" (Ctr (Ctr0 "Unit")))

in4 :: Exp
in4 = GCall1 Zero (ctr "S" [(ctr "S" [(ctr "Z" [ctr "Unit" []])])])
result4 = eval in4 predProg
test4 = result4 == Ctr (Ctr1 "Z" (Ctr (Ctr0 "Unit")))

tests = [test2, test3, test4]