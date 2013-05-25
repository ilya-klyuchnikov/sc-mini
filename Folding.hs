module Folding(foldTree) where

import Data
import DataUtil

foldTree :: Tree Expr -> Graph Expr
foldTree t = fixTree (tieKnot []) t

tieKnot :: [Node Expr] -> Node Expr -> Tree Expr -> Graph Expr
tieKnot ns n t@(Node e _) =
	case [(k, r) | k <- n:ns, isCall e, Just r <- [renaming (nodeLabel k) e]] of
		[] -> fixTree (tieKnot (n:ns)) t
		(k, r):_ -> Node e (EFold k r)
tieKnot ns n (Leaf e) = (Leaf e)

fixTree :: (Node t -> Tree t -> Graph t) -> Tree t -> Graph t
fixTree f (Node e (ETransient c)) = t where
	t = Node e $ ETransient $ f t c
fixTree f (Node e (EDecompose comp cs)) = t where 
	t = Node e $ EDecompose comp [f t c | c <- cs]
fixTree f (Node e (EVariants cs)) = t where
	t = Node e $ EVariants [(p, f t c) | (p, c) <- cs]
fixTree f (Leaf e) = (Leaf e)
