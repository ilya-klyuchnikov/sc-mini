module Interpreter where

import Data
import DataUtil

-- big-step int
eval :: Program -> Expr -> Expr
eval p (Ctr name args) = 
    Ctr name (map (eval p) args)

eval p (FCall name args) = 
    eval p (body // zip vs args) where
        (FDef _ vs body) = fDef p name

eval p (GCall gname (Ctr cname cargs : args)) = 
    eval p (body // zip (cvs ++ vs) (cargs ++ args)) where 
        (GDef _ (Pat _ cvs) vs body) = gDef p gname cname

eval p (GCall gname (arg:args)) = 
    eval p (GCall gname (eval p arg:args))

