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
simplify (Node e (EDecompose comp ts)) = 
	Node e (EDecompose comp $ map simplify ts)
simplify (Node e (EVariants cs)) = 
	Node e (EVariants [(c, simplify t) | (c, t) <- cs])
simplify (Node e (ETransient t)) | isBase e t = 
	Node e $ ETransient $ simplify t 
simplify (Node e (ETransient t)) = 
	simplify t
simplify t = t