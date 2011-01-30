module ATransformer where
	
import Data
import Driving
import Folding
import Generator

import Data.List

type Driver = Program -> NameSupply -> Expr -> Step Expr

-- One may say, that it is the simplest (yet very degenerative) supercompiler,
-- so we call it A transformer.
-- It fits the description of supercompiler, cited in the paper, but doesn't 
-- performs any simplifications.
transform :: State -> State
transform (expr, program) =
	residuate $ foldTree $ buildFTree drive program nameSupply expr

-- Build foldable tree, - ensures that the size of expressions
-- in nodes are limited (by whistle).
buildFTree :: Driver -> Program -> NameSupply -> Expr -> Tree
buildFTree d p (n:ns) e | whistle e = buildFTree d p ns $ generalize n e
buildFTree d p  ns    t | otherwise = case d p ns t of
	Decompose driven -> Node t $ Decompose (map (buildFTree d p ns) driven)
	Transient term -> Node t $ Transient (buildFTree d p ns term)
	Stop -> Node t Stop
	Variants cs -> 
		Node t $ Variants [(c, buildFTree d p (unused c ns) e) | (c, e) <- cs]
		
generalize :: Name -> Expr -> Expr
generalize n (FCall f es) = 
	Let n e (FCall f es') where (e, es') = gen n es
generalize n (GCall g es) = 
	Let n e (GCall g es') where (e, es') = gen n es

gen :: Name -> [Expr] -> (Expr, [Expr])	
gen n es = (maxE, vs ++ Var n : ws) where
	maxE = maximumBy (\x y -> compare (size x) (size y)) es
	(vs, w : ws) = break (maxE ==) es

whistle :: Expr -> Bool
whistle t@(FCall _ _) = size t > 100
whistle t@(GCall _ _) = size t > 100
whistle _ = False