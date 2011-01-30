module Deforester where

import Data
import Driving
import Folding
import Generator
import ATransformer

-- The feature of deforester is simplifying of 
-- folded tree.
deforest :: State -> State
deforest (e, p) =
	residuate $ simplify $ foldTree $ buildFTree drive p nameSupply e
	
simplify :: Tree -> Tree
simplify (Node e (Decompose ts)) = (Node e (Decompose $ map simplify ts))
simplify (Node e (Variants cs)) = Node e $ Variants [(c, simplify t) | (c, t) <- cs]
simplify (Node e (Transient t)) | isBase e t = Node e $ Transient $ simplify t 
simplify (Node e (Transient t)) = simplify t
simplify t = t