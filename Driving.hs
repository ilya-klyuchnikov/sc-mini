module Driving where
import Language
import Settings

data Contract = Contract String Pat deriving (Show)
data Step a = Transient a | Decompose [a] | Variants [(Contract, a)] | Stop | Fold deriving (Show)
data Tree = Node Term (Step Tree) deriving (Show)
type Variant a = (Contract, a)

nameSupply = ["$" ++ (show i) | i <- [1 ..] ]

buildTree :: Program -> NameSupply -> Term -> Tree
buildTree p ns t = case drive p ns t of
	Decompose driven -> Node t $ Decompose (map (buildTree p ns) driven)
	Variants cs -> Node t $ Variants [(c, buildTree p (drop (length vs) ns) t) | (c@(Contract _ (Pat _ vs)), t) <- cs]
	Transient term -> Node t $ Transient (buildTree p ns term)
	Stop -> Node t Stop

drive :: Program -> NameSupply -> Term -> Step Term
drive p ns (Var _) = Stop
drive p ns (Ctr _ []) = Stop
drive p ns (Ctr _ args) = Decompose args
drive p ns (Let x t1 t2) = Decompose [t1, t2]
drive p ns (FCall name args) = Transient (subst (zip vs args) t) where (FFun _ vs t) = fFun p name
drive p ns (GCall gname (v:vs)) = case driveG p ns gname v vs of
	Variants cs -> Variants $ map propagate cs
	s -> s
	
propagate :: (Contract, Term) -> (Contract, Term)
propagate (c@(Contract v (Pat cn vs)), t) | propagateInfo = (c, subst [(v, Ctr cn $ map Var vs)] t)
propagate (c, t) | otherwise = (c, t)

driveG :: Program -> NameSupply -> String -> Term -> [Term] -> Step Term
driveG p ns gname (Ctr cname cargs) args  = Transient (subst sub t) where 
	(GFun _ (Pat _ cvs) vs t) = gFun p gname cname
	sub = zip (cvs ++ vs) (cargs ++ args)
driveG p ns gname (Var v) args = Variants $ map (variant v ns args) (gFuns p gname)
driveG p ns gname inner args = proceed (drive p ns inner) where
	proceed (Transient t) = Transient (GCall gname (t:args));
	proceed (Variants cs) = Variants $ map (\(c, t) -> (c, GCall gname (t:args))) cs

variant :: String -> NameSupply -> [Term] -> GFun -> (Contract, Term)
variant v ns args (GFun _ (Pat cname cvs) vs body) = (Contract v (Pat cname fresh), subst sub body) where
	fresh = take (length cvs) ns
	sub = zip (cvs ++ vs) (map Var fresh ++ args)