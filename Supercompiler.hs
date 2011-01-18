module Supercompiler where

import Data
import Driving
import Settings
import Data.List

buildFoldableTree :: Program -> NameSupply -> Expr -> Tree
buildFoldableTree p (n:ns) t | whistle t = makeNode p ns $ generalize n t
                             | otherwise = makeNode p ns t

makeNode :: Program -> NameSupply -> Expr -> Tree
makeNode p ns t = case drive p ns t of
	Decompose driven -> Node t $ Decompose $ map (buildFoldableTree p ns) driven
	Variants cs -> Node t $ Variants [(c, buildFoldableTree p (unused c ns) t) | (c, t) <- cs]
	Transient term -> Node t $ Transient $ buildFoldableTree p ns term
	Stop -> Node t Stop
	
whistle :: Expr -> Bool
whistle t@(FCall _ _) = size t > maxConfSize
whistle t@(GCall _ _) = size t > maxConfSize
whistle _ = False

generalize :: String -> Expr -> Expr
generalize n (FCall f es) = Let n e (FCall f es') where (e, es') = gen n es
generalize n (GCall g es) = Let n e (GCall g es') where (e, es') = gen n es
		
gen n es = (maxE, vs ++ Var n : ws) where
	maxE = maximumBy (\x y -> compare (size x) (size y)) es
	(vs, w : ws) = break (maxE ==) es
