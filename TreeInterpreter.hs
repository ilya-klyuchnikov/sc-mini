module TreeInterpreter where

import Data
import DataUtil
import Maybe

intTree :: Tree -> Env -> Value
intTree (Node e Stop) env = 
	subst env e 

intTree (Node (Let (v, e1) e2) (Decompose [t1, t2])) env = 
	intTree t2 ((v, intTree t1 env) : env)

intTree (Node (Ctr cname _) (Decompose ts)) env =
	Ctr cname $ map (\t -> intTree t env) ts
	
intTree (Node _ (Transient t)) env = 
	intTree t env

intTree (Node e (Variants cs)) env = 
	 head $ catMaybes $ map (try env) cs

try :: Env -> (Contract, Tree) -> (Maybe Expr)
try env (Contract v (Pat pn vs), t) = 
	if cn == pn then (Just $ intTree t extendedEnv) else Nothing where 
		c@(Ctr cn args) = subst env (Var v)
		extendedEnv = (v, c) : zip vs args ++ env