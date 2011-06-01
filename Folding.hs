module Folding(foldTree) where

import Data
import DataUtil

foldTree :: Tree Conf -> Graph Conf
foldTree t = fixTree (tieKnot []) t

tieKnot :: [Node Conf] -> Node Conf -> Tree Conf -> Graph Conf
tieKnot ns n t@(Node e _) =
	case [(k, r) | k <- n:ns, isCall e, Just r <- [renaming (nodeLabel k) e]] of
		[] -> fixTree (tieKnot (n:ns)) t
		(k, r):_ -> Node e (Fold k r)
tieKnot ns n (Leaf e) = (Leaf e)

fixTree :: (Node t -> Tree t -> Graph t) -> Tree t -> Graph t
fixTree f (Node e (Transient c)) = t where
	t = Node e $ Transient $ f t c
fixTree f (Node e (Decompose comp cs)) = t where 
	t = Node e $ Decompose comp [f t c | c <- cs]
fixTree f (Node e (Variants cs)) = t where
	t = Node e $ Variants [(p, f t c) | (p, c) <- cs]
fixTree f (Leaf e) = (Leaf e)