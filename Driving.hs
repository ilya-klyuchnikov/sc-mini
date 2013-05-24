module Driving where

import Data
import DataUtil
import Interpreter

buildTree :: Machine Conf -> Conf -> Tree Conf
buildTree m e = bt m nameSupply e

bt :: Machine Conf -> NameSupply -> Conf -> Tree Conf
bt m ns c = case m ns c of
	Decompose comp ds -> Node c $ EDecompose comp (map (bt m ns) ds)
	Transient test e -> Node c $ ETransient test (bt m ns e)
	Stop e -> Leaf e
	Variants cs -> Node c $ EVariants [(c, bt m (unused c ns) e) | (c, e) <- cs]

driveMachine :: Program -> Machine Conf
driveMachine p = driveStep where
	driveStep :: Machine Conf
	driveStep ns e@(Var _) = 
		Stop e
	driveStep ns (GCall gn args) | isVar (head args) = 
		Variants (map (scrutinize ns args) (gDefs p gn)) 
	driveStep ns (GCall gn (arg:args)) | isCall arg = 
		case (driveStep ns arg) of
			Transient pat t -> Transient pat (GCall gn (t:args))
			Variants cs -> Variants (map (\(c, t) -> (c, GCall gn (t:args))) cs)
	driveStep ns (Let (x, t1) t2) = 
		Decompose (\[e1, e2] -> e2 // [(x, e1)]) [t1, t2]
	driveStep ns e =
		evalStep p e

scrutinize :: NameSupply -> [Expr] -> GDef -> (Contraction, Expr)
scrutinize ns (Var v : args) (GDef _ (Pat cn cvs) vs body) = 
	(Contraction v (Pat cn fresh), body // sub) where
		fresh = take (length cvs) ns
		sub = zip (cvs ++ vs) (map Var fresh ++ args)
