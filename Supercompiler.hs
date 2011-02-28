module Supercompiler where

import Data
import DataUtil
import Generator
import Folding
import Driving
import Prototype
import Deforester

supercompile :: Task -> Task
supercompile (expr, program) =
	residuate $ simplify $ foldTree $ buildFTree (addPropagation $ buildMachine program) expr

addPropagation :: Machine -> Machine
addPropagation dr = f where
	f ns e = propagateContract (dr ns e)

propagateContract :: Step Expr -> Step Expr
propagateContract (Variants vs) = 
	Variants [(c, subst [(v, Ctr cn $ map Var vs)] e) | (c@(Contract v (Pat cn vs)), e) <- vs]
propagateContract step = step