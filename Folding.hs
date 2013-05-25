module Folding(foldTree) where

import Data
import DataUtil

foldTree :: Tree Expr -> Graph Expr
foldTree t = fixTree (tieKnot []) t

tieKnot :: [Node Expr] -> Node Expr -> Tree Expr -> Graph Expr
tieKnot ns n t@(Node e _) =
	case [(k, r) | k <- n:ns, isCall e, Just r <- [renaming (nodeLabel k) e]] of
		[] -> fixTree (tieKnot (n:ns)) t
		(k, r):_ -> Node e (Fold k r)

-- here is a trick with recursive values: t where t = .... t
fixTree :: (Node t -> Tree t -> Graph t) -> Tree t -> Graph t
fixTree f (Node e (Transient c)) = t where
	t = Node e $ Transient $ f t c
fixTree f (Node e (Decompose n cs)) = t where 
	t = Node e $ Decompose n [f t c | c <- cs]
fixTree f (Node e (Variants cs)) = t where
	t = Node e $ Variants [(p, f t c) | (p, c) <- cs]
fixTree f (Node e Stop) = (Node e Stop)