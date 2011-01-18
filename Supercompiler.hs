module Supercompiler where

import Language
import Driving
import Data.List

maxSize = 8

buildFoldableTree :: Program -> NameSupply -> Term -> Tree
buildFoldableTree p (n:ns) t | whistle t = makeNode p ns $ generalize n t
                             | otherwise = makeNode p ns t

makeNode :: Program -> NameSupply -> Term -> Tree
makeNode p ns t = case drive p ns t of {
	DecomposeStep driven -> Node t $ DecomposeStep (map (buildFoldableTree p ns) driven);
	ContractStep cs -> Node t $ ContractStep (map (\(c@(Contract _ (Pat _ vs)), t) -> (c, buildFoldableTree p (drop (length vs) ns) t)) cs);
	TransientStep term -> Node t $ TransientStep (buildFoldableTree p ns term);
	Stop -> Node t Stop;}
	
whistle :: Term -> Bool
whistle t@(FCall _ _) = size t > maxSize
whistle t@(GCall _ _) = size t > maxSize
whistle _ = False

generalize :: String -> Term -> Term
generalize n (FCall f es) = Let n e (FCall f es') where (e, es') = gen n es
generalize n (GCall g es) = Let n e (GCall g es') where (e, es') = gen n es
		
gen n es = (maxE, vs ++ Var n : ws) where
	maxE = maximumBy (\x y -> compare (size x) (size y)) es
	(vs, w : ws) = break (maxE ==) es

size :: Term -> Int
size (Var _) = 1
size (Ctr _ args) = 1 + sum (map size args)
size (FCall _ args) = 1 + sum (map size args)
size (GCall _ args) = 1 + sum (map size args)
size (Let _ e1 e2) = 1 + (size e1) + (size e2)
