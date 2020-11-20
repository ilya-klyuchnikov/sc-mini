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
addPropagation m ns e = propagateContract (m ns e)

propagateContract :: Step Conf -> Step Conf
propagateContract (Variants vs) =
  Variants [(c, e // [(v, Ctr cn $ map Var vs)]) |
            (c@(Contract v (Pat cn vs)), e) <- vs]
propagateContract step = step
