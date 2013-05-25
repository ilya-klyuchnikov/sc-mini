module Driving where

import Data
import DataUtil
import Interpreter

buildTree :: Machine Expr -> Expr -> Tree Expr
buildTree m c = case m c of
	Decompose ds -> Node c $ EDecompose $ map (buildTree m) ds
	Transient e -> Node c $ ETransient $ buildTree m e
	Stop e -> Leaf e
	Variants cs -> Node c $ EVariants [(c, buildTree m e) | (c, e) <- cs]

driveMachine :: Program -> Machine Expr
driveMachine p e@(Var _) = 
	Stop e
driveMachine p (GCall gn args@(Var _ : _)) = 
	Variants (map (scrutinize args) (gDefs p gn)) 
driveMachine p (GCall gn (arg:args)) | isCall arg = 
	case (driveMachine p arg) of
		Transient t -> Transient (GCall gn (t:args))
		Variants cs -> Variants (map (\(c, t) -> (c, GCall gn (t:args))) cs)
driveMachine p e = evalStep p e

scrutinize :: [Expr] -> GDef -> (Contraction, Expr)
scrutinize (Var v : args) (GDef _ (Pat cn cvs) vs body) = 
	(Contraction v (Pat cn fresh), body // sub) where
		ids = take (length cvs) $ iterate (+1) 1
		fresh = map (\x -> SVar (cn ++ "_" ++ (show x)) v) ids
		sub = zip (cvs ++ vs) (map Var fresh ++ args)
