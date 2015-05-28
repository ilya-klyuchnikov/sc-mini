module Treeless1 where

import Debug.Trace
 
-- syntax for treeless programs
data Exp = FVar1 | GVar1 | FCall1 String Exp | GCall1 String Exp | Ctr Ctr deriving (Show, Eq)
data Ctr = Ctr0 String | Ctr1 String Exp | Ctr2 String Exp Exp deriving (Show, Eq)

data Fun = FFun1 String Exp | GFun1 String String Exp
type Program = [Fun]

eval :: Exp -> Program -> Exp        
eval (Ctr c) p         = Ctr (evalCtr c p)
eval (FCall1 n ctr) p  = evalFCall1 p p n ctr
eval (GCall1 gn ctr) p = evalGCall1 ctr p gn 

evalCtr :: Ctr -> Program -> Ctr
evalCtr (Ctr0 s) p       = Ctr0 s
evalCtr (Ctr1 s e) p     = Ctr1 s (eval e p)
evalCtr (Ctr2 s e1 e2) p = Ctr2 s (eval e1 p) (eval e2 p)

eval10 :: Exp -> Program -> Exp -> Exp
eval10 FVar1 p fv1 = fv1
eval10 (Ctr c) p fv1 = Ctr (evalCtr10 c p fv1)
eval10 (FCall1 n FVar1) p fv1 = evalFCall1 p p n fv1
-- TODO - check via (isVar) -- should work!!
eval10 (FCall1 n ctr) p fv1 = evalFCall1 p p n ctr
eval10 (GCall1 n FVar1) p fv1 = evalGCall1 fv1 p n
eval10 (GCall1 n ctr) p fv1 = evalGCall1 ctr p n 

eval01 :: Exp -> Program -> Exp -> Exp
eval01 GVar1 p gv1 = gv1
eval01 (Ctr c) p gv1 = Ctr (evalCtr01 c p gv1)
-- TODO - check via (isVar) -- should work!!
eval01 (FCall1 n GVar1) p gv1 = evalFCall1 p p n gv1
eval01 (FCall1 n ctr) p gv1  = evalFCall1 p p n ctr
-- TODO - check via (isVar) -- should work!!
eval01 (GCall1 n GVar1) p gv1 = evalGCall1 gv1 p n
eval01 (GCall1 n ctr) p gv1 = evalGCall1 ctr p n 

evalCtr10 :: Ctr -> Program -> Exp -> Ctr
evalCtr10 (Ctr0 s) p fv1 = Ctr0 s
evalCtr10 (Ctr1 s e) p fv1 = Ctr1 s (eval10 e p fv1)
evalCtr10 (Ctr2 s e1 e2) p fv1 = Ctr2 s (eval10 e1 p fv1) (eval10 e2 p fv1)

evalCtr01 :: Ctr -> Program -> Exp -> Ctr
evalCtr01 (Ctr0 s) p fv1 = Ctr0 s
evalCtr01 (Ctr1 s e) p fv1 = Ctr1 s (eval01 e p fv1)
evalCtr01 (Ctr2 s e1 e2) p fv1 = Ctr2 s (eval01 e1 p fv1) (eval01 e2 p fv1)

evalFCall1 :: Program -> Program -> String -> Exp -> Exp
evalFCall1 p1 p fname arg1 = 
    case p1 of
        (FFun1 n' e) : p' -> 
            if (n'==fname) 
                then (eval10 e p arg1) 
                else (evalFCall1 p' p fname arg1)
        (GFun1 _ _ _) : p' -> 
            (evalFCall1 p' p fname arg1)

evalGCall1 :: Exp -> Program -> String -> Exp
evalGCall1 (Ctr (Ctr1 cn arg1)) p gn = evalGCall1' p p gn cn arg1

evalGCall1' :: Program -> Program -> String -> String -> Exp -> Exp
evalGCall1' p1 p gn cn arg1 =
    case p1 of
        (GFun1 gn' cn' e) : p' -> 
            if (gn'==gn && cn'==cn) 
                then (eval01 e p arg1) 
                else (evalGCall1' p' p gn cn arg1)
        (FFun1 _ _) : p' -> 
            (evalGCall1' p' p gn cn arg1)


-------- examples ---------

id1 v1 = id2 v1
id2 v1 = id3 v1
id3 v1 = v1

idProg :: Program
idProg = [
    FFun1 "id1" (FCall1 "id2" FVar1),
    FFun1 "id2" FVar1
    ]

data Unit = U
data Nat = S Nat | Z Unit

pred (S g1) = g1
pred (Z g1) = (Z g1)
zero (S g1) = zero g1
zero (Z g1) = (Z g1)

predProg :: Program
predProg = [
    GFun1 "pred" "S" GVar1,
    GFun1 "pred" "Z" (ctr "Z" [GVar1]),
    GFun1 "zero" "S" (GCall1 "zero" (GVar1)),
    GFun1 "zero" "Z" (ctr "Z" [GVar1])
    ]

ctr :: String -> [Exp] -> Exp
ctr s [] = Ctr (Ctr0 s)
ctr s [e1] = Ctr (Ctr1 s e1)
ctr s [e1, e2] = Ctr (Ctr2 s e1 e2)

in1 :: Exp
in1 = FCall1 "id1" (ctr "Nil" [])
result1 = eval in1 idProg
test1 = result1 == Ctr (Ctr0 "Nil")

in2 :: Exp
in2 = GCall1 "pred" (ctr "Z" [ctr "Unit" []])
result2 = eval in2 predProg
test2 = result2 == Ctr (Ctr1 "Z" (Ctr (Ctr0 "Unit")))

in3 :: Exp
in3 = GCall1 "pred" (ctr "S" [(ctr "Z" [ctr "Unit" []])])
result3 = eval in3 predProg
test3 = result3 == Ctr (Ctr1 "Z" (Ctr (Ctr0 "Unit")))

in4 :: Exp
in4 = GCall1 "zero" (ctr "S" [(ctr "S" [(ctr "Z" [ctr "Unit" []])])])
result4 = eval in4 predProg
test4 = result4 == Ctr (Ctr1 "Z" (Ctr (Ctr0 "Unit")))

tests = [test1, test2, test3, test4]