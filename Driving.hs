module Driving where

import Data
import DataUtil

-- Builds an infinite (in a general case) process tree.
buildTree :: Driver -> NameSupply -> Expr -> Tree
buildTree drive ns t = case drive ns t of
	Decompose driven -> Node t $ Decompose (map (buildTree drive ns) driven)
	Transient term -> Node t $ Transient (buildTree drive ns term)
	Stop -> Node t Stop
	Variants cs -> 
		Node t $ Variants [(c, buildTree drive (unused c ns) e) | (c, e) <- cs]

-- Models a behavior of interpreter.
drive :: DriverGen
drive p ns (Var _) = Stop
drive p ns (Ctr _ []) = Stop
drive p ns (Ctr _ args) = Decompose args
drive p ns (Let (x, t1) t2) = Decompose [t1, t2]
drive p ns (FCall name args) = Transient $ subst (zip vs args) e where 
	FFun _ vs e = fFun p name
drive p ns (GCall gname ((Ctr cname cargs):args)) = Transient (subst sub t) where
	(GFun _ (Pat _ cvs) vs t) = gFun p gname cname
	sub = zip (cvs ++ vs) (cargs ++ args)
drive p ns (GCall gname ((Var v):args)) = Variants $ map (variant v ns args) (gFuns p gname)
drive p ns (GCall gname (inner:args)) = proceed (drive p ns inner) where
	proceed (Transient t) = Transient (GCall gname (t:args))
	proceed (Variants cs) = Variants [(c, GCall gname (t:args)) | (c, t) <- cs]

variant :: Name -> NameSupply -> [Expr] -> GFun -> (Contract, Expr)
variant v ns args (GFun _ (Pat cname cvs) vs body) = (Contract v (Pat cname fresh), subst sub body) where
	fresh = take (length cvs) ns
	sub = zip (cvs ++ vs) (map Var fresh ++ args)