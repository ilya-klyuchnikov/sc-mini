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
nan m e1 e2 = filter (\(k, v) -> not (Var k == v)) sub where
	sub0 = map (\n -> (n, Var n)) (vnames e2)
	sub = nan' sub0 (buildTree m e1) (buildTree m e2)

-- sub is already calculated "contractions" for universal variables
nan' :: Subst -> Tree Conf -> Tree Conf -> Subst
nan' sub (Node _ (EDecompose _ ts1)) (Node _ (EDecompose _ ts2)) = sub' where
	subs = zipWith (nan' sub) ts1 ts2
	sub' = foldl1 intersectSubst subs
nan' sub (Node _ (ETransient _ t1)) (Node _ (ETransient _ t2)) = 
	nan' sub t1 t2
nan' sub (Node _ (ETransient (Just (Match (Pat cname _))) t1)) (Node conf (EVariants xs)) = sub' where
	Just (Contraction v pat, t2) = find (\(Contraction _ (Pat cn _), _) -> cn == cname) xs
	sub' = nan' (sub /// [(v, pat2Ctr pat)]) t1 t2
-- in the case of conveniality class, 
-- we should return just sub
nan' sub (Leaf e1) (Leaf (Var n)) = 
	sub /// [(n, e1)]
nan' sub _ _ = 
	sub

-- compose substitution
(///) :: Subst -> Subst -> Subst
(///) sub1 sub2 = (map (\(k, v) -> (k, v // sub2)) sub1)

-- by design k1 == k2
intersectSubst :: Subst -> Subst -> Subst
intersectSubst = zipWith (\(k1, val1) (k2, val2) -> (k1, intersectCtr val1 val2))

-- intersects Contstructors (c-expressions)
-- assumption: 
--  c1 and c2 should be "intersectable"
intersectCtr :: Conf -> Conf -> Conf
intersectCtr (Ctr n1 args1) (Ctr _ args2) =
	Ctr n1 (zipWith intersectCtr args1 args2)
intersectCtr (Var _) c2 = c2
intersectCtr c1 (Var _) = c1