module Prototype where
	
import Data
import DataUtil
import Driving
import Folding
import Generator
import List

-- One may say, that it is the simplest (yet very degenerative) supercompiler,
-- so we call it a prototype.
-- It fits the description of supercompiler, cited in the paper, but doesn't 
-- perform any simplifications so far.
transform :: Task -> Task
transform (expr, program) =
	residuate $ foldTree $ buildFTree (buildMachine program) expr

-- Build foldable tree, - ensures that the size of expressions
-- in nodes are limited (by whistle).

buildFTree :: Machine -> Expr -> Tree
buildFTree m e = buildFTree' m nameSupply e

buildFTree' :: Machine -> NameSupply -> Expr -> Tree
buildFTree' d (n:ns) e | whistle e = buildFTree' d ns $ generalize n e
buildFTree' d ns     t | otherwise = case d ns t of
	Decompose driven -> Node t $ Decompose (map (buildFTree' d ns) driven)
	Transient term -> Node t $ Transient (buildFTree' d ns term)
	Stop -> Node t Stop
	Variants cs -> 
		Node t $ Variants [(c, buildFTree' d (unused c ns) e) | (c, e) <- cs]

maxExpSize = 40
-- The simplest ad-hoc whistle: it limits the size of an expression.
whistle :: Expr -> Bool
whistle e@(FCall _ args) = not (all isVar args) && size e > maxExpSize
whistle e@(GCall _ args) = not (all isVar args) && size e > maxExpSize
whistle _ = False

-- The simplest ad-hoc generalization: the largest argument of
-- a function call is extracted.
generalize :: Name -> Expr -> Expr
generalize n (FCall f es) = 
	Let (n, e) (FCall f es') where (e, es') = gen n es
generalize n (GCall g es) = 
	Let (n, e) (GCall g es') where (e, es') = gen n es

-- Helper for generalize.
gen :: Name -> [Expr] -> (Expr, [Expr])	
gen n es = (maxE, vs ++ Var n : ws) where
	maxE = maximumBy (\x y -> compare ((eType x) * (size x)) ((eType y)*(size y))) es
	(vs, w : ws) = break (maxE ==) es
	
eType (Var _) = 0
eType _ = 1