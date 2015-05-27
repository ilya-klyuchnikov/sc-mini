module Treeless1 where

import Debug.Trace
 
-- syntax for treeless programs
data Exp = FVar Int | GVar Int | FCall1 String Exp | GCall1 String Exp | Ctr Ctr deriving (Show, Eq)
data Ctr = Ctr0 String | Ctr1 String Exp | Ctr2 String Exp Exp deriving (Show, Eq)

data Fun = FFun1 String Exp | GFun1 String String Exp
type Program = [Fun]

eval :: Program -> Exp -> Exp
eval p e = 
    case e of        
        Ctr c -> Ctr (evalCtr p c)
        FCall1 n ctr -> evalFCall1 p p n ctr
        GCall1 gn ctr -> evalGCall1 p gn ctr

evalCtr :: Program -> Ctr -> Ctr
evalCtr p ctr =
    case ctr of 
        Ctr0 s   -> Ctr0 s
        Ctr1 s e -> Ctr1 s (eval p e)
        Ctr2 s e1 e2 -> Ctr2 s (eval p e1) (eval p e2)

eval10 :: Program -> Exp -> Exp -> Exp
eval10 p e fv1 =
    case e of
        FVar 1 -> fv1
        Ctr c -> Ctr (evalCtr10 p c fv1)
        FCall1 n (FVar 1) -> evalFCall1 p p n fv1
        FCall1 n ctr      -> evalFCall1 p p n ctr
        GCall1 n (FVar 1) -> evalGCall1 p n fv1
        GCall1 n ctr      -> evalGCall1 p n ctr

eval01 :: Program -> Exp -> Exp -> Exp
eval01 p e gv1 =
    case e of
        GVar 1 -> gv1
        Ctr c -> Ctr (evalCtr01 p c gv1)
        FCall1 n (GVar 1) -> evalFCall1 p p n gv1
        FCall1 n ctr      -> evalFCall1 p p n ctr
        GCall1 n (GVar 1) -> evalGCall1 p n gv1
        GCall1 n ctr      -> evalGCall1 p n ctr

evalCtr10 :: Program -> Ctr -> Exp -> Ctr
evalCtr10 p c fv1 =
    case c of 
        Ctr0 s   -> Ctr0 s
        Ctr1 s e -> Ctr1 s (eval10 p e fv1)
        Ctr2 s e1 e2 -> Ctr2 s (eval10 p e1 fv1) (eval10 p e2 fv1)

evalCtr01 :: Program -> Ctr -> Exp -> Ctr
evalCtr01 p c gv1 =
    case c of 
        Ctr0 s   -> Ctr0 s
        Ctr1 s e -> Ctr1 s (eval01 p e gv1)
        Ctr2 s e1 e2 -> Ctr2 s (eval01 p e1 gv1) (eval01 p e2 gv1)


evalFCall1 :: Program -> Program -> String -> Exp -> Exp
evalFCall1 p p1 fname arg1 = 
    case p1 of
        (FFun1 n' e) : p' -> 
            if (n'==fname) 
                then (eval10 p e arg1) 
                else (evalFCall1 p p' fname arg1)
        _ : p' -> 
            (evalFCall1 p p' fname arg1)

evalGCall1 :: Program -> String -> Exp -> Exp
evalGCall1 p gn (Ctr (Ctr1 cn arg1)) = 
    evalGCall1' p p gn cn arg1

evalGCall1' :: Program -> Program -> String -> String -> Exp -> Exp
evalGCall1' p p1 gn cn arg1 =
    case p1 of
        (GFun1 gn' cn' e) : p' -> 
            if (gn'==gn && cn'==cn) 
                then (eval01 p e arg1) 
                else (evalGCall1' p p' gn cn arg1)
        _ : p' -> 
            (evalGCall1' p p' gn cn arg1)


-------- examples ---------

id1 v1 = id2 v1
id2 v1 = id3 v1
id3 v1 = v1

idProg :: Program
idProg = [
    FFun1 "id1" (FCall1 "id2" (FVar 1)),
    FFun1 "id2" (FVar 1)
    ]

data Unit = U
data Nat = S Nat | Z Unit

pred (S g1) = g1
pred (Z g1) = (Z g1)
zero (S g1) = zero g1
zero (Z g1) = (Z g1)

predProg :: Program
predProg = [
    GFun1 "pred" "S" (GVar 1),
    GFun1 "pred" "Z" (ctr "Z" [GVar 1]),
    GFun1 "zero" "S" (GCall1 "zero" (GVar 1)),
    GFun1 "zero" "Z" (ctr "Z" [GVar 1])
    ]

ctr :: String -> [Exp] -> Exp
ctr s [] = Ctr (Ctr0 s)
ctr s [e1] = Ctr (Ctr1 s e1)
ctr s [e1, e2] = Ctr (Ctr2 s e1 e2)

in1 :: Exp
in1 = FCall1 "id1" (ctr "Nil" [])
result1 = eval idProg in1
test1 = result1 == Ctr (Ctr0 "Nil")

in2 :: Exp
in2 = GCall1 "pred" (ctr "Z" [ctr "Unit" []])
result2 = eval predProg in2
test2 = result2 == Ctr (Ctr1 "Z" (Ctr (Ctr0 "Unit")))

in3 :: Exp
in3 = GCall1 "pred" (ctr "S" [(ctr "Z" [ctr "Unit" []])])
result3 = eval predProg in3
test3 = result3 == Ctr (Ctr1 "Z" (Ctr (Ctr0 "Unit")))

in4 :: Exp
in4 = GCall1 "zero" (ctr "S" [(ctr "S" [(ctr "Z" [ctr "Unit" []])])])
result4 = eval predProg in4
test4 = result4 == Ctr (Ctr1 "Z" (Ctr (Ctr0 "Unit")))

tests = [test1, test2, test3, test4]