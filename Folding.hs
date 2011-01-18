module Folding(foldTree) where

import Data
import Driving
import Data.List
import Data.Maybe

-- folding (without generalization) of an infinite tree into a graph
foldTree :: Tree -> Tree
foldTree t = tieKnot [] t

tieKnot :: [Tree] -> Tree -> Tree
tieKnot ts t@(Node e _) = tied where
	tied:_ = [Node e (Fold k r) | k@(Node b _) <- ts, Just r <- [renaming e b]] ++ [(traverse ts t)]
	
traverse :: [Tree] -> Tree -> Tree
traverse ts (Node e (Transient c)) = t where
	t = Node e $ Transient $ tieKnot (t:ts) c
traverse ts (Node e (Decompose cs)) = t where 
	t = Node e $ Decompose [tieKnot (t:ts) c | c <- cs]
traverse ts (Node e (Variants cs)) = t where
	t = Node e $ Variants [(c, tieKnot (t:ts) v) | (c, v) <- cs]
traverse ts (Node e Stop) = (Node e Stop)