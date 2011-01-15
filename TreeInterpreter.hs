module TreeInterpreter where

import Driving
import Language
import Data.Maybe

intTree :: Subst -> Tree -> Term
intTree sub (Node e Stop) = 
	subst sub e 

intTree sub (Node (Let v e1 e2) (DecomposeStep [t1, t2])) = 
	intTree ((v, intTree sub t1) : sub) t2

intTree sub (Node (Ctr cname _) (DecomposeStep ts)) =
	Ctr cname $ map (intTree sub) ts
	
intTree sub (Node _ (TransientStep t)) = 
	intTree sub t

intTree sub (Node e (ContractStep cs)) = 
	 head $ catMaybes $ map (try sub) cs

try :: Subst -> (Contract, Tree) -> (Maybe Term)
try sub (Contract v (Pat pn vs), t) = 
	if cn == pn then (Just $ intTree extendedSub t) else Nothing where 
		c@(Ctr cn args) = subst sub (Var v)
		extendedSub = (v, c) : zip vs args ++ sub