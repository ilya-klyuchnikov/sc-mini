module Driving where

import Data
import DataUtil

buildTree :: Machine Conf -> Conf -> Tree Conf
buildTree m e = bt m nameSupply e

bt :: Machine Conf -> NameSupply -> Conf -> Tree Conf
bt m ns c = case m ns c of
	Decompose comp ds -> Node c $ EDecompose comp (map (bt m ns) ds)
	Transient e -> Node c $ ETransient (bt m ns e)
	Stop e -> Leaf e
	Variants cs -> Node c $ EVariants [(c, bt m (unused c ns) e) | (c, e) <- cs]

driveMachine :: Program -> Machine Conf
driveMachine p = drive where
	drive :: Machine Conf
	drive ns e@(Var _) = Stop e
	drive ns e@(Ctr _ []) = Stop e
	drive ns (Ctr n args) = Decompose (Ctr n) args
	drive ns (Let (x, t1) t2) = Decompose (\[e1, e2] -> e2 // [(x, e1)] ) [t1, t2]
	drive ns (FCall name args) = Transient $ e // (zip vs args) where 
		FDef _ vs e = fDef p name
	drive ns (GCall gn (Ctr cn cargs : args)) = Transient $ e // sub where
		(GDef _ (Pat _ cvs) vs e) = gDef p gn cn
		sub = zip (cvs ++ vs) (cargs ++ args)
	drive ns (GCall gn args@((Var _):_)) = Variants $ variants gn args where
		variants gn args = map (scrutinize ns args) (gDefs p gn)
	drive ns (GCall gn (inner:args)) = inject (drive ns inner) where
		inject (Transient t) = Transient (GCall gn (t:args))
		inject (Variants cs) = Variants $ map f cs 
		f (c, t) = (c, GCall gn (t:args))

scrutinize :: NameSupply -> [Expr] -> GDef -> (Contract, Expr)
scrutinize ns (Var v : args) (GDef _ (Pat cn cvs) vs body) = 
	(Contract v (Pat cn fresh), body // sub) where
		fresh = take (length cvs) ns
		sub = zip (cvs ++ vs) (map Var fresh ++ args)