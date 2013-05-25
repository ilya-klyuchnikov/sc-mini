module Interpreter where

import Data
import DataUtil

-- small-step int
int :: Program -> Expr -> Expr
int p e = until isValue (intStep p) e

intStep :: Program -> Expr -> Expr
intStep p (Ctr name args) = 
	Ctr name (values ++ (intStep p x : xs)) where 
		(values, x : xs) = span isValue args

intStep p (FCall name args) = 
	body // zip vs args where 
		(FDef _ vs body) = fDef p name

intStep p (GCall gname (Ctr cname cargs : args)) = 
	body // zip (cvs ++ vs) (cargs ++ args) where 
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

intStep p (GCall gname (e:es)) = 
	(GCall gname (intStep p e : es))

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

