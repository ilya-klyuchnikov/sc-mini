module Supercompiler where

import Data
import DataUtil
import Generator
import Folding
import Driving
import Prototype
import Deforester

supercompile :: Task -> Task
supercompile (e, p) =
  residuate $ simplify $ foldTree $ buildFTree (addPropagation $ driveMachine p) e

addPropagation :: Machine Conf -> Machine Conf
addPropagation m ns e = propagateContraction (m ns e)

propagateContraction :: Step Conf -> Step Conf
propagateContraction (Variants vs) = 
  Variants [(c, e // [(v, Ctr cn $ map Var vs)]) | 
            (c@(Contraction v (Pat cn vs)), e) <- vs]
propagateContraction step = step
