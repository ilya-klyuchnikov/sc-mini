module Interpreter where

import Data
import DataUtil

evalStep :: Program -> Expr -> Step Expr
evalStep p (Ctr name []) =
	Stop (Ctr name [])

evalStep p (Ctr name args) = 
	Decompose args

evalStep p (FCall name args) = 
	Transient (body // zip vs args) where
		(FDef _ vs body) = fDef p name

evalStep p (GCall gname ((Ctr cname cargs) : args)) = 
	Transient (body // sub) where 
		(GDef _ pat@(Pat _ cvs) vs body) = gDef p gname cname
		sub = zip (cvs ++ vs) (cargs ++ args)

evalStep p (GCall gname (arg:args)) = 
	case evalStep p arg of 
		Transient arg' -> Transient (GCall gname (arg':args)) where

-- OLD STUFF FURTHER --

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

		
sll_trace :: Task -> Subst -> (Expr, Integer)
sll_trace (e, prog) s = intC prog (e // s)

intC :: Program -> Expr -> (Expr, Integer) 
intC p e = until t f (e, 0) where
	t (e, n) = isValue e
	f (e, n) = (intStep p e, n + 1)
