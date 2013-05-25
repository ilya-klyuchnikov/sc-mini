module Driving where

import Data
import DataUtil
import Interpreter

buildTree :: Machine Conf -> Conf -> Tree Conf
buildTree m c = case m c of
	Decompose comp ds -> Node c $ EDecompose comp $ map (buildTree m) ds
	Transient tr e -> Node c $ ETransient tr $ buildTree m e
	Stop e -> Leaf e
	Variants cs -> Node c $ EVariants [(c, buildTree m e) | (c, e) <- cs]

driveMachine :: Program -> Machine Conf
driveMachine p = driveStep where
	driveStep :: Machine Conf
	driveStep e@(Var _) = 
		Stop e
	driveStep (GCall gn args) | isVar (head args) = 
		Variants (map (scrutinize args) (gDefs p gn)) 
	driveStep (GCall gn (arg:args)) | isCall arg = 
		case (driveStep arg) of
			Transient pat t -> Transient pat (GCall gn (t:args))
			Variants cs -> Variants (map (\(c, t) -> (c, GCall gn (t:args))) cs)
	driveStep e =
		evalStep p e

scrutinize :: [Expr] -> GDef -> (Contraction, Expr)
scrutinize (Var v : args) (GDef _ (Pat cn cvs) vs body) = 
	(Contraction v (Pat cn fresh), body // sub) where
		ids = take (length cvs) $ iterate (+1) 1
		fresh = map (\x -> SVar (cn ++ "_" ++ (show x)) v) ids
		sub = zip (cvs ++ vs) (map Var fresh ++ args)
