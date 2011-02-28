module Interpreter where

import Data
import DataUtil

sll_run :: Task -> Env -> Value
sll_run (e, program) env = int program (subst env e)

-- small-step semantics
int :: Program -> Expr -> Value
int p e | isValue e = e
        | otherwise = int p (intStep p e)

intStep :: Program -> Expr -> Expr
intStep p (Ctr name args) = 
	Ctr name (values ++ (intStep p x : xs)) where 
		(values, x : xs) = span isValue args

intStep p (FCall name args) = 
	(subst (zip vs args) t) where 
		(FDef _ vs t) = fDef p name

intStep p (GCall gname (Ctr cname cargs : args)) = 
	subst (zip (cvs ++ vs) (cargs ++ args)) t where 
		(GDef _ (Pat _ cvs) vs t) = gDef p gname cname

intStep p (GCall gname (e:es)) = 
	(GCall gname (intStep p e : es))
	
intStep p (Let binding e2) =
	subst [binding] e2

-- big-step semantics
eval :: Program -> Expr -> Expr
eval p (Ctr name args) = 
	Ctr name [eval p arg | arg <- args]

eval p (FCall name args) = 
	subst (zip vs [eval p arg | arg <- args]) body where
		(FDef _ vs body) = fDef p name

eval p (GCall gname args) = 
	subst (zip (cvs ++ vs) (cargs ++ gargs)) body where
		(Ctr cname cargs) : gargs = [eval p arg | arg <- args]
		(GDef _ (Pat _ cvs) vs body) = gDef p gname cname

eval p (Let (x, e1) e2) =
	subst [(x, eval p e1)] (eval p e2)

		
sll_trace :: Task -> Subst -> (Value, Integer)
sll_trace (e, prog) s = intC prog (subst s e)

intC :: Program -> Expr -> (Expr, Integer) 
intC p e = intC' p (e, 0) 
intC' p (e, n) | isValue e = (e, n)
               | otherwise = intC' p (intStep p e, n + 1)

log_sll_trace :: Program -> Expr -> IO()
log_sll_trace p e | isValue e = putStrLn (show e)
                  | otherwise = do
                    putStrLn (show e)
                    log_sll_trace p (intStep p e)