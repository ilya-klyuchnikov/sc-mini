module Deforester where

import Data
import DataUtil
import Driving
import Folding

--deforest :: Task -> Task
--deforest (e, p) =
--	residuate $ simplify $ foldTree $ buildTree (driveMachine p) e
	
simplify :: Graph Expr -> Graph Expr
simplify (Node e (EDecompose ts)) = 
	Node e (EDecompose $ map simplify ts)
simplify (Node e (EVariants cs)) = 
	Node e (EVariants [(c, simplify t) | (c, t) <- cs])
simplify (Node e (ETransient t)) | isBase e t = 
	Node e $ ETransient $ simplify t 
simplify (Node e (ETransient t)) = 
	simplify t
simplify t = t

isBase e1 (Node _ (EDecompose ts)) = or $ map (isBase e1) ts
isBase e1 (Node _ (EVariants cs)) = or $ map (isBase e1 . snd) cs 
isBase e1 (Node _ (ETransient t)) = isBase e1 t
isBase e1 (Node _ (EFold (Node e2 _) _)) = e1 == e2
isBase e1 (Leaf e2) = False
