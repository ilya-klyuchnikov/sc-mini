module Supercompiler where

import Data
import Generator
import Folding
import Driving
import Settings
import Data.List

supercompile :: Config -> State -> State
supercompile (Config shouldSimplify shouldPropogate sizeBound) (expr, program) =
	residuate $ simplify $ foldTree $ buildFTree program nameSupply expr

buildFTree :: Program -> NameSupply -> Expr -> Tree
buildFTree p (n:ns) t | whistle t = traverse p ns $ generalize n t
                      | otherwise = traverse p ns t

traverse :: Program -> NameSupply -> Expr -> Tree
traverse p ns t = case drive p ns t of
	Decompose driven -> Node t $ Decompose $ map (buildFTree p ns) driven
	Variants cs -> Node t $ Variants [(c, buildFTree p (unused c ns) tuned) 
		| (c, e) <- cs, let tuned = propagateContract c e]
	Transient term -> Node t $ Transient $ buildFTree p ns term
	Stop -> Node t Stop
	
whistle :: Expr -> Bool
whistle t@(FCall _ _) = size t > maxConfSize
whistle t@(GCall _ _) = size t > maxConfSize
whistle _ = False

generalize :: Name -> Expr -> Expr
generalize n (FCall f es) = Let n e (FCall f es') where (e, es') = gen n es
generalize n (GCall g es) = Let n e (GCall g es') where (e, es') = gen n es
		
gen n es = (maxE, vs ++ Var n : ws) where
	maxE = maximumBy (\x y -> compare (size x) (size y)) es
	(vs, w : ws) = break (maxE ==) es
	
simplify :: Tree -> Tree
simplify (Node e (Decompose ts)) = (Node e (Decompose $ map simplify ts))
simplify (Node e (Variants cs)) = Node e $ Variants [(c, simplify t) | (c, t) <- cs]
simplify (Node e (Transient t@(Node e1 _))) | isBase e t = 
	if isBase e1 t then Node e $ Transient $ simplify t else Node e (step (simplify t))
simplify (Node e (Transient t)) = simplify t
simplify t = t
