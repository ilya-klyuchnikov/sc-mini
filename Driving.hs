module Driving where
import Language
import Settings

data Contract = Contract String Pat deriving (Show)
data Step a = Transient a | Decompose [a] | Variants [(Contract, a)] | Stop | Fold a deriving (Show)
data Tree = Node Expr (Step Tree) deriving (Show)
type Variant a = (Contract, a)

nameSupply = ["$" ++ (show i) | i <- [1 ..] ]

buildTree :: Program -> NameSupply -> Expr -> Tree
buildTree p ns t = case drive p ns t of
	Decompose driven -> Node t $ Decompose (map (buildTree p ns) driven)
	Variants cs -> Node t $ Variants [(c, buildTree p ns1 $ propagate c e) 
		| (c, e) <- cs, let ns1 = unused c ns]
	Transient term -> Node t $ Transient (buildTree p ns term)
	Stop -> Node t Stop

drive :: Program -> NameSupply -> Expr -> Step Expr
drive p ns (Var _) = Stop
drive p ns (Ctr _ []) = Stop
drive p ns (Ctr _ args) = Decompose args
drive p ns (Let x t1 t2) = Decompose [t1, t2]
drive p ns (FCall name args) = Transient $ subst (zip vs args) e where FFun _ vs e = fFun p name
drive p ns (GCall gname (v:vs)) = driveG p ns gname v vs
	
propagate :: Contract -> Expr -> Expr
propagate (Contract v (Pat cn vs)) e | propagateInfo = subst [(v, Ctr cn $ map Var vs)] e
propagate c e | otherwise = e

unused (Contract _ (Pat _ vs)) = drop (length vs)

driveG :: Program -> NameSupply -> String -> Expr -> [Expr] -> Step Expr
driveG p ns gname (Ctr cname cargs) args  = Transient (subst sub t) where 
	(GFun _ (Pat _ cvs) vs t) = gFun p gname cname
	sub = zip (cvs ++ vs) (cargs ++ args)
driveG p ns gname (Var v) args = Variants $ map (variant v ns args) (gFuns p gname)
driveG p ns gname inner args = proceed (drive p ns inner) where
	proceed (Transient t) = Transient (GCall gname (t:args));
	proceed (Variants cs) = Variants [(c, GCall gname (t:args)) | (c, t) <- cs]

variant :: String -> NameSupply -> [Expr] -> GFun -> (Contract, Expr)
variant v ns args (GFun _ (Pat cname cvs) vs body) = (Contract v (Pat cname fresh), subst sub body) where
	fresh = take (length cvs) ns
	sub = zip (cvs ++ vs) (map Var fresh ++ args)
	
expr (Node e _) = e