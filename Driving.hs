module Driving where
import Language

data Contract = Contract String Pat deriving (Show)
data Step a = TransientStep a | DecomposeStep [a] | ContractStep [(Contract, a)] | Stop | Fold deriving (Show)
data Tree = Node Term (Step Tree) deriving (Show)
type Variant a = (Contract, a)

nameSupply = ["$" ++ (show i) | i <- [1 ..] ]

buildTree :: Program -> NameSupply -> Term -> Tree
buildTree p ns t = case drive p ns t of {
	DecomposeStep driven -> Node t $ DecomposeStep (map (buildTree p ns) driven);
	ContractStep cs -> Node t $ ContractStep [(c, buildTree p (drop (length vs) ns) t) | (c@(Contract _ (Pat _ vs)), t) <- cs];
	TransientStep term -> Node t $ TransientStep (buildTree p ns term);
	Stop -> Node t Stop;}

drive :: Program -> NameSupply -> Term -> Step Term
drive p ns (Var _) = Stop
drive p ns (Ctr _ []) = Stop
drive p ns (Ctr _ args) = DecomposeStep args
drive p ns (Let x t1 t2) = DecomposeStep [t1, t2]
drive p ns (FCall name args) = TransientStep (subst (zip vs args) t) where (FFun _ vs t) = fFun p name
drive p ns (GCall gname (v:vs)) = case driveG p ns gname v vs of
	ContractStep cs -> ContractStep [(c, subst [(v, Ctr cn $ map Var vs)] t) | (c@(Contract v (Pat cn vs)), t) <- cs]
	s -> s

driveG :: Program -> NameSupply -> String -> Term -> [Term] -> Step Term
driveG p ns gname (Ctr cname cargs) args  = TransientStep (subst sub t) where 
	(GFun _ (Pat _ cvs) vs t) = gFun p gname cname
	sub = zip (cvs ++ vs) (cargs ++ args)
driveG p ns gname (Var v) args = ContractStep $ map (variant v ns args) (gFuns p gname)
driveG p ns gname inner args = proceed (drive p ns inner) where
	proceed (TransientStep t) = TransientStep (GCall gname (t:args));
	proceed (ContractStep cs) = ContractStep $ map (\(c, t) -> (c, GCall gname (t:args))) cs
	
variant :: String -> NameSupply -> [Term] -> GFun -> (Contract, Term)
variant v ns args (GFun _ (Pat cname cvs) vs body) = (Contract v (Pat cname fresh), subst sub body) where
	fresh = take (length cvs) ns
	sub = zip (cvs ++ vs) (map Var fresh ++ args)