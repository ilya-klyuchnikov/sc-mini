module Treeless where

import Debug.Trace
 
-- syntax for treeless programs
data Ptr = Ptr0 String | Ptr1 String | Ptr2 String deriving (Show)
data Exp = GVar Int | FVar Int | FCall String [Exp] | GCall String [Exp] | Ctr Ctr deriving (Show)
data Ctr = Ctr0 String | Ctr1 String [Exp] | Ctr2 String [Exp] deriving (Show)

data Fun = FFun Int String Exp | GFun Int String Ptr Exp deriving (Show)
type Program = [Fun]
 
-- interpreter for deforestable programs (and expressions!)
-- cons-free, almost tail-recursive program (only evalCtr is not tail-recursive)
eval :: Program -> Exp -> [Exp] -> [Exp] -> Exp
eval p e fenv genv = 
    trace ("\n<<" ++ show e ++ "/" ++ (show fenv) ++ (show genv) ++ ">>\n")
    (case e of 
        -- TODO - possibly, we should suppose that this is a value!
        FVar i -> eval p (fenv !! i) fenv genv -- (fenv !! i)
        GVar i -> eval p (genv !! i) fenv genv -- (genv !! i)
        Ctr c -> Ctr (evalCtr p c fenv genv)
        FCall n args -> evalFCall p p n args
        GCall gname (arg0 : args) -> evalGCall p gname arg0 args fenv genv)
 
evalCtr p c fenv genv =
    case c of 
        Ctr0 s   -> Ctr0 s
        Ctr1 s [e] -> Ctr1 s [eval p e fenv genv]
        Ctr2 s [e1, e2] -> Ctr2 s [(eval p e1 fenv genv), (eval p e2 fenv genv)]

-- here is the trick - note that no additional data is produced at all!! 
-- here is the problem!!
evalFCall :: Program -> Program -> String -> [Exp] -> Exp
evalFCall p p1 fname fargs = 
    case p1 of
        (FFun _ n' e) : p' -> 
            if (n'==fname) 
                then (eval p e fargs []) 
                else (evalFCall p p' fname fargs)
        _ : p' -> 
            (evalFCall p p' fname fargs)

evalGCall p gname arg0 args fenv genv =
    trace ("\n<<!" ++ show (GCall gname (arg0:args)) ++ "/" ++ (show fenv) ++ (show genv) ++ "!>>\n")
    (case arg0 of
        FVar i -> evalGCall p gname (fenv !! i) args fenv genv
        GVar i -> evalGCall p gname (genv !! i) args fenv genv
        Ctr c ->  evalGCall' p gname c args)

evalGCall' p gname arg0 fargs =
    case arg0 of
        Ctr0 ctrname -> evalGCall0 p p gname ctrname fargs
        Ctr1 ctrname gargs -> evalGCall1 p p gname ctrname fargs gargs
        Ctr2 ctrname gargs -> evalGCall2 p p gname ctrname fargs gargs 

evalGCall0 :: Program -> Program -> String -> String -> [Exp] -> Exp
evalGCall0 p0 p gname ctrname fargs = 
    trace ("\n<<000" ++ show (GCall gname ((Ctr (Ctr0 ctrname)) : fargs)) ++ "???" ++ (show p) ++ "!>>\n")
    (case p of
        (GFun _ n' (Ptr0 s) e) : p' -> 
            if (n'==gname && s == ctrname) 
                then (eval p0 e fargs []) 
                else (evalGCall0 p0 p' gname ctrname fargs)
        _ : p' -> 
            (evalGCall0 p0 p' gname ctrname fargs))

evalGCall1 :: Program -> Program -> String -> String -> [Exp] -> [Exp] -> Exp
evalGCall1 p0 p gname ctrname fargs gargs = 
    case p of
        (GFun _ n' (Ptr1 s) e) : p' -> 
            if (n'==gname && s == ctrname) 
                then (eval p0 e fargs gargs) 
                else (evalGCall1 p0 p' gname ctrname fargs gargs)
        _ : p' -> 
            (evalGCall1 p0 p' gname ctrname fargs gargs)

evalGCall2 :: Program -> Program -> String -> String -> [Exp] -> [Exp] -> Exp
evalGCall2 p0 p gname ctrname fargs gargs = 
    case p of
        (GFun _ n' (Ptr2 s) e) : p' -> 
            if (n'==gname && s == ctrname) 
                then (eval p0 e fargs gargs) 
                else (evalGCall2 p0 p' gname ctrname fargs gargs)
        _ : p' -> 
            (evalGCall2 p0 p' gname ctrname fargs gargs)
