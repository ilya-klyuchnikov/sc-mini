module TreeInterpreter where

import Data
import List
import DataUtil
import Maybe

intTree :: Tree Conf -> Env -> Value
intTree (Node e Stop) env = 
	e // env
intTree (Node (Ctr cname _) (Decompose ts)) env =
	Ctr cname $ map (\t -> intTree t env) ts
intTree (Node _ (Transient t)) env = 
	intTree t env
intTree (Node e (Variants cs)) env = 
	head $ catMaybes $ map (try env) cs
intTree (Node (Let (v, e1) e2) (Decompose [t1, t2])) env =
	intTree t2 ((v, intTree t1 env) : env)
intTree (Node _ (Fold t ren)) env = 
	intTree t $ map (\(k, v) -> (renKey k, v)) env where
		renKey k = maybe k fst (find ((k ==) . snd)  ren)

try :: Env -> (Contract, Tree Conf) -> (Maybe Expr)
try env (Contract v (Pat pn vs), t) = 
	if cn == pn then (Just $ intTree t extEnv) else Nothing where 
		c@(Ctr cn cargs) = (Var v) // env
		extEnv = zip vs cargs ++ env