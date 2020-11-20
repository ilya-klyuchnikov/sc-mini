module Deforester where

import Data
import DataUtil
import Driving
import Folding
import Generator
import Prototype

deforest :: Task -> Task
deforest (e, p) =
  residuate $ simplify $ foldTree $ buildFTree (driveMachine p) e

simplify :: Graph Conf -> Graph Conf
simplify (Node e (Decompose ts)) =
  Node e (Decompose $ map simplify ts)
simplify (Node e (Variants cs)) =
  Node e (Variants [(c, simplify t) | (c, t) <- cs])
simplify (Node e (Transient t)) | isBase e t =
  Node e $ Transient $ simplify t
simplify (Node e (Transient t)) =
  simplify t
simplify t = t
