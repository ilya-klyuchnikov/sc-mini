module Driving where

import Data
import DataUtil

buildTree :: Machine Conf -> Conf -> Tree Conf
buildTree m e = bt m nameSupply e

bt :: Machine Conf -> NameSupply -> Conf -> Tree Conf
bt m ns c = case m ns c of
	Decompose comp ds -> Node c $ EDecompose comp (map (bt m ns) ds)
	Transient contr e -> Node c $ ETransient (bt m ns e)
	Stop e -> Leaf e
	Variants cs -> Node c $ EVariants [(c, bt m (unused c ns) e) | (c, e) <- cs]

driveMachine :: Program -> Machine Conf
driveMachine p = drive where
	drive :: Machine Conf
	-- extra
	drive ns e@(Var _) = Stop e
	-- = eval
	drive ns (Ctr n args) = 
		Decompose (Ctr n) args
	-- = eval
	drive ns (FCall name args) = 
		Transient Nothing $ e // (zip vs args) where 
			FDef _ vs e = fDef p name
	-- = eval
	drive ns (GCall gn (Ctr cn cargs : args)) = Transient (Just pat) (e // sub) where
		(GDef _ pat@(Pat _ cvs) vs e) = gDef p gn cn
		sub = zip (cvs ++ vs) (cargs ++ args)
	-- extra
	drive ns (Let (x, t1) t2) = Decompose (\[e1, e2] -> e2 // [(x, e1)] ) [t1, t2]
	-- extra
	drive ns (GCall gn args@((Var _):_)) = Variants $ variants gn args where
		variants gn args = map (scrutinize ns args) (gDefs p gn)
	-- extra
	drive ns (GCall gn (inner:args)) = inject (drive ns inner) where
		inject (Transient pat t) = Transient pat (GCall gn (t:args))
		inject (Variants cs) = Variants $ map f cs 
		f (c, t) = (c, GCall gn (t:args))

scrutinize :: NameSupply -> [Expr] -> GDef -> (Contraction, Expr)
scrutinize ns (Var v : args) (GDef _ (Pat cn cvs) vs body) = 
	(Contraction v (Pat cn fresh), body // sub) where
		fresh = take (length cvs) ns
		sub = zip (cvs ++ vs) (map Var fresh ++ args)