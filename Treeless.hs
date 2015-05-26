module Treeless where
 
-- syntax for treeless programs
data Ptr = Ptr0 String | Ptr1 String | Ptr2 String
data Exp = GVar Int | FVar Int | FCall String [Exp] | GCall String [Exp] | Ctr Ctr
data Ctr = Ctr0 String | Ctr1 String [Exp] | Ctr2 String [Exp]

data Fun = FFun Int String Exp | GFun Int String Ptr Exp 
type Program = [Fun]
 
-- interpreter for deforestable programs (and expressions!)
-- cons-free, almost tail-recursive program (only evalCtr is not tail-recursive)
eval :: Program -> Exp -> [Exp] -> [Exp] -> Exp
eval p e fenv genv = 
    case e of 
        -- TODO - possibly, we should suppose that this is a value!
        FVar i -> eval p (fenv !! i) fenv genv -- (fenv !! i)
        GVar i -> eval p (genv !! i) fenv genv -- (genv !! i)
        Ctr c -> Ctr (evalCtr p c fenv genv)
        FCall n args -> evalFCall p n args
        GCall gname ((Ctr (Ctr0 ctrname)) : fargs) -> evalGCall0 p gname ctrname fargs
        GCall gname ((Ctr (Ctr1 ctrname gargs)) : fargs) -> evalGCall1 p gname ctrname fargs gargs
        GCall gname ((Ctr (Ctr2 ctrname gargs)) : fargs) -> evalGCall2 p gname ctrname fargs gargs
 
evalCtr p c fenv genv =
    case c of 
        Ctr0 s   -> Ctr0 s
        Ctr1 s [e] -> Ctr1 s [eval p e fenv genv]
        Ctr2 s [e1, e2] -> Ctr2 s [(eval p e1 fenv genv), (eval p e2 fenv genv)]

-- here is the trick - note that no additional data is produced at all!! 
evalFCall :: Program -> String -> [Exp] -> Exp
evalFCall p fname fargs = 
    case p of
        (FFun _ n' e) : p' -> 
            if (n'==fname) 
                then (eval p e fargs []) 
                else (evalFCall p' fname fargs)
        _ : p' -> 
            (evalFCall p' fname fargs)

evalGCall0 :: Program -> String -> String -> [Exp] -> Exp
evalGCall0 p gname ctrname fargs = 
    case p of
        (GFun _ n' (Ptr0 s) e) : p' -> 
            if (n'==gname && s == ctrname) 
                then (eval p e fargs []) 
                else (evalGCall0 p' gname ctrname fargs)
        _ : p' -> 
            (evalGCall0 p' gname ctrname fargs)

evalGCall1 :: Program -> String -> String -> [Exp] -> [Exp] -> Exp
evalGCall1 p gname ctrname fargs gargs = 
    case p of
        (GFun _ n' (Ptr1 s) e) : p' -> 
            if (n'==gname && s == ctrname) 
                then (eval p e fargs gargs) 
                else (evalGCall1 p' gname ctrname fargs gargs)
        _ : p' -> 
            (evalGCall1 p' gname ctrname fargs gargs)

evalGCall2 :: Program -> String -> String -> [Exp] -> [Exp] -> Exp
evalGCall2 p gname ctrname fargs gargs = 
    case p of
        (GFun _ n' (Ptr2 s) e) : p' -> 
            if (n'==gname && s == ctrname) 
                then (eval p e fargs gargs) 
                else (evalGCall2 p' gname ctrname fargs gargs)
        _ : p' -> 
            (evalGCall2 p' gname ctrname fargs gargs)
