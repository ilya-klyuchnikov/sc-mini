module Folding(foldTree) where

import Data
import DataUtil
import Driving

-- folding of foldable infinite tree into a graph
foldTree :: Tree -> Tree
foldTree t = tieKnot [] t

-- we tie a knot only for calls
-- it is enough in the first-order settings
tieKnot :: [Tree] -> Tree -> Tree
tieKnot ts t@(Node e _) = n where
	n:_ = [Node e (Fold k r) | k <- ts, Just r <- [renaming (expr k) e], isCall e] ++ [(traverse ts t)]
	
traverse :: [Tree] -> Tree -> Tree
traverse ts (Node e (Transient c)) = t where
	t = Node e $ Transient $ tieKnot (t:ts) c
traverse ts (Node e (Decompose cs)) = t where 
	t = Node e $ Decompose [tieKnot (t:ts) c | c <- cs]
traverse ts (Node e (Variants cs)) = t where
	t = Node e $ Variants [(c, tieKnot (t:ts) v) | (c, v) <- cs]
traverse ts (Node e Stop) = (Node e Stop)