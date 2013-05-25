module TreeInterpreter where

import Data
import Data.List
import DataUtil
import Data.Maybe

intTree :: Tree Expr -> Subst -> Expr
intTree (Leaf e) env = 
	e // env
intTree (Node (Ctr cname _) (EDecompose comp ts)) env =
	comp $ map (\t -> intTree t env) ts
intTree (Node _ (ETransient t)) env = 
	intTree t env
intTree (Node e (EVariants cs)) env = 
	head $ catMaybes $ map (try env) cs
intTree (Node _ (EFold t ren)) env = 
	intTree t $ map (\(k, v) -> (renKey k, v)) env where
		renKey k = maybe k fst (find ((k ==) . snd)  ren)

try :: Subst -> (Contraction, Tree Expr) -> (Maybe Expr)
try env (Contraction v (Pat pn vs), t) = 
	if cn == pn then (Just $ intTree t extEnv) else Nothing where 
		c@(Ctr cn cargs) = (Var v) // env
		extEnv = zip vs cargs ++ env
