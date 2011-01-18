module TreeInterpreter where

import Driving
import Language
import Data.Maybe

intTree :: Subst -> Tree -> Term
intTree env (Node e Stop) = 
	subst env e 

intTree env (Node (Let v e1 e2) (Decompose [t1, t2])) = 
	intTree ((v, intTree env t1) : env) t2

intTree env (Node (Ctr cname _) (Decompose ts)) =
	Ctr cname $ map (intTree env) ts
	
intTree env (Node _ (Transient t)) = 
	intTree env t

intTree env (Node e (Variants cs)) = 
	 head $ catMaybes $ map (try env) cs

try :: Subst -> (Contract, Tree) -> (Maybe Term)
try env (Contract v (Pat pn vs), t) = 
	if cn == pn then (Just $ intTree extendedEnv t) else Nothing where 
		c@(Ctr cn args) = subst env (Var v)
		extendedEnv = (v, c) : zip vs args ++ env