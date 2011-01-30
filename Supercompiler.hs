module Supercompiler where

import Data
import Generator
import Folding
import Driving
import ATransformer
import Deforester

supercompile :: State -> State
supercompile (expr, program) =
	residuate $ simplify $ foldTree $ buildFTree pimpedDrive program nameSupply expr where
		pimpedDrive p ns e = propagateContract (drive p ns e)

propagateContract :: Step Expr -> Step Expr
propagateContract (Variants vs) = 
	Variants [(c, subst [(v, Ctr cn $ map Var vs)] e) | (c@(Contract v (Pat cn vs)), e) <- vs]
propagateContract step = step