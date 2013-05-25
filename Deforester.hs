module Deforester where

import Data
import DataUtil
import Driving
import Folding

--deforest :: Task -> Task
--deforest (e, p) =
--	residuate $ simplify $ foldTree $ buildTree (driveMachine p) e
	
simplify :: Graph Expr -> Graph Expr
simplify (Node e (Decompose n ts)) = 
	Node e (Decompose n $ map simplify ts)
simplify (Node e (Variants cs)) = 
	Node e (Variants [(c, simplify t) | (c, t) <- cs])
simplify (Node e (Transient t)) | isBase e t = 
	Node e $ Transient $ simplify t 
simplify (Node e (Transient t)) = 
	simplify t
simplify t = t

isBase e1 (Node _ (Decompose _ ts)) = or $ map (isBase e1) ts
isBase e1 (Node _ (Variants cs)) = or $ map (isBase e1 . snd) cs 
isBase e1 (Node _ (Transient t)) = isBase e1 t
isBase e1 (Node _ (Fold (Node e2 _) _)) = e1 == e2
isBase e1 (Node e2 Stop) = False
