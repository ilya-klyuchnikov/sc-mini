module Deforester where

import Data
import DataUtil
import Driving
import Folding
import Generator

deforest :: Task -> Task
deforest (e, p) =
	residuate $ simplify $ foldTree $ buildTree (driveMachine p) e
	
simplify :: Graph Conf -> Graph Conf
simplify (Node e (EDecompose comp ts)) = 
	Node e (EDecompose comp $ map simplify ts)
simplify (Node e (EVariants cs)) = 
	Node e (EVariants [(c, simplify t) | (c, t) <- cs])
simplify (Node e (ETransient tr t)) | isBase e t = 
	Node e $ ETransient tr $ simplify t 
simplify (Node e (ETransient _ t)) = 
	simplify t
simplify t = t
