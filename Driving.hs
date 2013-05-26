module Driving where

import Data
import DataUtil

buildTree :: Driving a -> a -> Tree a
buildTree m c = case m c of
	Decompose n ds -> Node c $ Decompose n $ map (buildTree m) ds
	Transient e -> Node c $ Transient $ buildTree m e
	Stop -> Node c Stop
	Variants cs -> Node c $ Variants $ map (\(c, e) ->(c, buildTree m e)) cs

driveExpr :: Program -> Driving Expr
-- treeless
driveExpr p (Var _) = 
	Stop
-- treeless
driveExpr p (Ctr name []) =
	Stop
-- treeless
driveExpr p (Ctr name args) = 
	Decompose name args
driveExpr p (FCall name args) = 
	Transient (body // zip vs args) where
		(FDef _ vs body) = fDef p name
driveExpr p (GCall gn args@(Var _ : _)) = 
	Variants (map (scrutinize args) (gDefs p gn)) 
driveExpr p (GCall gname ((Ctr cname cargs) : args)) = 
	Transient (body // sub) where 
		(GDef _ pat@(Pat _ cvs) vs body) = gDef p gname cname
		sub = zip (cvs ++ vs) (cargs ++ args)
driveExpr p (GCall gn (arg:args)) = 
	case (driveExpr p arg) of
		Transient t -> Transient (GCall gn (t:args))
		Variants cs -> Variants (map (\(c, t) -> (c, GCall gn (t:args))) cs)

scrutinize :: [Expr] -> GDef -> (Contraction, Expr)
scrutinize (Var v : args) (GDef _ (Pat cn cvs) vs body) = 
	(Contraction v (Pat cn fresh), body // sub) where
		ids = take (length cvs) $ iterate (+1) 1
		fresh = map (\x -> SVar (cn ++ "_" ++ (show x)) v) ids
		sub = zip (cvs ++ vs) (map Var fresh ++ args)
