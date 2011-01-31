module Supercompiler where

import Data
import Generator
import Folding
import Driving
import ATransformer
import Deforester

-- The main feature of supercompiler is propagation of information.
supercompile :: State -> State
supercompile (expr, program) =
	residuate $ simplify $ foldTree $ buildFTree (propagate drive) program nameSupply expr

-- Pimp driving with positive information propagation.
propagate :: Drive -> Drive
propagate dr = f where
	f p ns e = propagateContract (dr p ns e)

propagateContract :: Step Expr -> Step Expr
propagateContract (Variants vs) = 
	Variants [(c, subst [(v, Ctr cn $ map Var vs)] e) | (c@(Contract v (Pat cn vs)), e) <- vs]
propagateContract step = step