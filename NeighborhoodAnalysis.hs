module NeighborhoodAnalysis(nan) where

import Data
import DataUtil
import Driving
import Data.List

-- assumptions:
--  1) m constructs a perfect process tree
--  2) e1 is an instance of e2 (e2 < e1)
--  3) (eval p e1) terminates
nan :: Machine Conf -> Expr -> Conf -> Subst
nan m e1 e2 = nan' sub0 (buildTree m e1) (buildTree m e2') where
	sub0 = zip (vnames e2) freshVars
	e2' = e2 // sub0

nan' :: Subst -> Tree Conf -> Tree Conf -> Subst
nan' sub (Node _ (EDecompose _ ts1)) (Node _ (EDecompose _ ts2)) = 
	concat $ zipWith (nan' sub) ts1 ts2
nan' sub (Node _ (ETransient _ t1)) (Node _ (ETransient _ t2)) = 
	nan' sub t1 t2
nan' sub (Node _ (ETransient (Just (Match (Pat cname _))) t1)) (Node conf (EVariants xs)) = sub2 where
	-- find corresponding branch
	Just (Contraction v pat, t2) = find (\(Contraction _ (Pat cn _), _) -> cn == cname) xs
	-- get corresponding class (substitution)
	sub1 = compSub [(v, pat2Ctr pat)] (nan' sub t1 t2)
	-- remove substituted components
	sub2 = filter (\kv -> elem (fst kv) (vnames conf)) sub1
nan' sub (Leaf e1) (Leaf (Var n)) = 
	[(n, e1)]
nan' sub _ _ = 
	sub

-- compose substitution
compSub :: Subst -> Subst -> Subst
compSub mainSub sub1 = mainSub' ++ sub1 where
	mainSub' = map (\(k, v) -> (k, v // sub1)) mainSub

intersectSubst :: Subst -> Subst -> Subst
intersectSubst = undefined

-- intersects Contstructors (c-expressions)
-- assumption: 
--  c1 and c2 should be "intersectable"
intersectCtr :: Conf -> Conf -> Conf
intersectCtr (Ctr n1 args1) (Ctr _ args2) =
	Ctr n1 (zipWith intersectCtr args1 args2)
intersectCtr (Var _) c2 = c2
intersectCtr c1 (Var _) = c1