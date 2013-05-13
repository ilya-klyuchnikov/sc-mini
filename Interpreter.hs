module Interpreter where

import Data
import DataUtil

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
	
intStep p (Let (x, e1) e2) =
	e2 // [(x, e1)]

eval :: Program -> Expr -> Expr
eval p (Ctr name args) = 
	Ctr name [eval p arg | arg <- args]

eval p (FCall name args) = 
	eval p (body // zip vs args) where
		(FDef _ vs body) = fDef p name

eval p (GCall gname (Ctr cname cargs : args)) = 
	eval p (body // zip (cvs ++ vs) (cargs ++ args)) where 
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

eval p (GCall gname (arg:args)) = 
	eval p (GCall gname (eval p arg:args))

eval p (Let (x, e1) e2) =
	eval p (e2 // [(x, e1)])

sll_run :: Task -> Env -> Value
sll_run (e, program) env = int program (e // env)
		
sll_trace :: Task -> Subst -> (Value, Integer)
sll_trace (e, prog) s = intC prog (e // s)

intC :: Program -> Expr -> (Expr, Integer) 
intC p e = until t f (e, 0) where
	t (e, n) = isValue e
	f (e, n) = (intStep p e, n + 1)
