module Driving where

import Data
import DataUtil

-- Builds an infinite (in a general case) process tree using a provided state machine.

buildTree :: Machine -> Expr -> Tree
buildTree m e = buildTree' m nameSupply e

buildTree' :: Machine -> NameSupply -> Expr -> Tree
buildTree' m ns t = case m ns t of
	Decompose driven -> Node t $ Decompose (map (buildTree' m ns) driven)
	Transient term -> Node t $ Transient (buildTree' m ns term)
	Stop -> Node t Stop
	Variants cs -> 
		Node t $ Variants [(c, buildTree' m (unused c ns) e) | (c, e) <- cs]

buildMachine :: MachineGen
buildMachine p = drive where
	drive :: Machine
	drive ns (Var _) = Stop
	drive ns (Ctr _ []) = Stop
	drive ns (Ctr _ args) = Decompose args
	drive ns (Let (x, t1) t2) = Decompose [t1, t2]
	drive ns (FCall name args) = Transient $ subst (zip vs args) e where 
		FFun _ vs e = fFun p name
	drive ns (GCall gname ((Ctr cname cargs):args)) = Transient $ subst sub t where
		(GFun _ (Pat _ cvs) vs t) = gFun p gname cname
		sub = zip (cvs ++ vs) (cargs ++ args)
	drive ns (GCall gname args@((Var _):_)) = Variants $ variants gname args  where
		variants :: Name -> [Expr] -> [(Contract, Expr)]
		variants gname args = map (scrutinize ns args) (gFuns p gname)
	drive ns (GCall gname (inner:args)) = inject (drive ns inner) where
		inject (Transient t) = Transient (GCall gname (t:args))
		inject (Variants cs) = Variants [(c, GCall gname (t:args)) | (c, t) <- cs]

scrutinize :: NameSupply -> [Expr] -> GFun -> (Contract, Expr)
scrutinize ns ((Var v):args) (GFun _ (Pat cn cvs) vs body) = (Contract v (Pat cn fresh), subst sub body) where
	fresh = take (length cvs) ns
	sub = zip (cvs ++ vs) (map Var fresh ++ args)