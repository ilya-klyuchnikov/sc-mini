module TreeInterpreter where

import Data
import DataUtil
import Maybe

intTree :: Tree Conf -> Env -> Value
intTree (Node e Stop) env = 
	e // env
intTree (Node (Let (v, e1) e2) (Decompose [t1, t2])) env = 
	intTree t2 ((v, intTree t1 env) : env)
intTree (Node (Ctr cname _) (Decompose ts)) env =
	Ctr cname $ map (\t -> intTree t env) ts
intTree (Node _ (Transient t)) env = 
	intTree t env
intTree (Node e (Variants cs)) env = 
	 head $ catMaybes $ map (try env) cs

try :: Env -> (Contract, Tree Conf) -> (Maybe Expr)
try env (Contract v (Pat pn vs), t) = 
	if cn == pn then (Just $ intTree t extEnv) else Nothing where 
		c@(Ctr cn args) = (Var v) // env
		extEnv = (v, c) : zip vs args ++ env