module Interpreter where

import Data
import DataUtil

type Value = Expr

intFacade :: Task -> Subst -> (Value, Integer)
intFacade (e, prog) s = intC prog (subst s e)

int :: Program -> Expr -> Expr
int p e | isValue e = e
        | otherwise = int p (intStep p e)

intStep :: Program -> Expr -> Expr
intStep p (Ctr name args) = 
	Ctr name (values ++ (intStep p x : xs)) where 
		(values, x : xs) = span isValue args

intStep p (FCall name args) = 
	(subst (zip vs args) t) where 
		(FFun _ vs t) = fFun p name

intStep p (GCall gname (Ctr cname cargs : args)) = 
	subst (zip (cvs ++ vs) (cargs ++ args)) t where 
		(GFun _ (Pat _ cvs) vs t) = gFun p gname cname

intStep p (GCall gname (e:es)) = 
	(GCall gname (intStep p e : es))
	
intStep p (Let x e1 e2) =
	subst [(x, e1)] e2

isValue :: Expr -> Bool
isValue (Ctr _ args) = and $ map isValue args 
isValue _ = False

intC :: Program -> Expr -> (Expr, Integer) 
intC p e = intC' p (e, 0) 
intC' p (e, n) | isValue e = (e, n)
               | otherwise = intC' p (intStep p e, n + 1)

-- only for tracing
traceInt :: Program -> Expr -> IO()
traceInt p e | isValue e = putStrLn (show e)
			 | otherwise = do
				putStrLn (show e)
				traceInt p (intStep p e)