module Treeless1 where

import Debug.Trace
 
-- syntax for treeless programs
data Exp = FVar Int | FCall1 String Exp | GCall1 String Exp | Ctr Ctr deriving (Show)
data Ctr = Ctr0 String | Ctr1 String [Exp] | Ctr2 String [Exp] deriving (Show)

data Fun = FFun1 String Exp | GFun1 String String Exp
type Program = [Fun]

eval :: Program -> Exp -> Exp
eval p e = 
    case e of        
        Ctr c -> Ctr (evalCtr p c)
        FCall1 n arg -> evalFCall1 p p n arg

evalCtr :: Program -> Ctr -> Ctr
evalCtr p ctr =
    case ctr of 
        Ctr0 s   -> Ctr0 s
        Ctr1 s [e] -> Ctr1 s [eval p e]
        Ctr2 s [e1, e2] -> Ctr2 s [(eval p e1), (eval p e2)]

eval10 :: Program -> Exp -> Exp -> Exp
eval10 p e fv1 =
    case e of
        FVar 1 -> fv1 -- or: just v1
        Ctr c -> Ctr (evalCtr10 p c fv1)
        FCall1 n (FVar 1) -> evalFCall1 p p n fv1
        FCall1 n ctr      -> evalFCall1 p p n ctr

evalCtr10 :: Program -> Ctr -> Exp -> Ctr
evalCtr10 p c fv1 =
    case c of 
        Ctr0 s   -> Ctr0 s
        Ctr1 s [e] -> Ctr1 s [eval10 p e fv1]
        Ctr2 s [e1, e2] -> Ctr2 s [(eval10 p e1 fv1), (eval10 p e2 fv1)]

evalFCall1 :: Program -> Program -> String -> Exp -> Exp
evalFCall1 p p1 fname arg1 = 
    case p1 of
        (FFun1 n' e) : p' -> 
            if (n'==fname) 
                then (eval10 p e arg1) 
                else (evalFCall1 p p' fname arg1)
        _ : p' -> 
            (evalFCall1 p p' fname arg1)

-------- examples ---------

id1 v1 = id2 v1
id2 v1 = id3 v1
id3 v1 = v1

idProg :: Program
idProg = [
    FFun1 "id1" (FCall1 "id2" (FVar 1)),
    FFun1 "id2" (FVar 1)
    ]

ctr :: String -> [Exp] -> Exp
ctr s [] = Ctr (Ctr0 s)
ctr s [e1] = Ctr (Ctr1 s [e1])
ctr s [e1, e2] = Ctr (Ctr2 s [e1, e2])

in10 :: Exp
in10 = FCall1 "id1" (ctr "Nil" [])
result10 = eval idProg in10