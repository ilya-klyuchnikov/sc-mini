module Treeless where

import Debug.Trace
 
-- IN PROGRESS - not working for now 
-- syntax for treeless programs
data Exp = GVar Int | FVar Int | FCall1 String Exp | Ctr Ctr deriving (Show)
data Ctr = Ctr0 String | Ctr1 String [Exp] | Ctr2 String [Exp] deriving (Show)

data Fun = FFun1 String Exp
type Program = [Fun]

eval :: Program -> Exp -> Exp
eval = undefined
 
-- interpreter for deforestable programs (and expressions!)
-- eval for combination [binding] []
eval10 :: Program -> Exp -> Exp -> Exp
eval10 p e v1 =
    case e of
        FVar 0 -> eval p v1
        Ctr c -> Ctr (evalCtr10 p c v1)
        FCall1 n (FVar _) -> evalFCall1 p p n v1
        FCall1 n ctr      -> evalFCall1 p p n ctr
 
evalCtr10 p c v1 =
    case c of 
        Ctr0 s   -> Ctr0 s
        Ctr1 s [e] -> Ctr1 s [eval10 p e fenv genv]
        Ctr2 s [e1, e2] -> Ctr2 s [(eval p e1 fenv genv), (eval p e2 fenv genv)]

-- here is the trick - note that no additional data is produced at all!! 
-- here is the problem!!
evalFCall1 :: Program -> Program -> String -> Exp -> Exp
evalFCall1 p p1 fname arg1 = 
    case p1 of
        (FFun1 n' e) : p' -> 
            if (n'==fname) 
                then (eval p e [arg1] []) 
                else (evalFCall1 p p' fname arg1)
        _ : p' -> 
            (evalFCall1 p p' fname arg1)
