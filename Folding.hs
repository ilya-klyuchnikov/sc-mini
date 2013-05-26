module Folding(foldTreeExpr, foldTreeStack) where

import Data
import DataUtil
import Stack

foldTreeExpr :: Tree Expr -> Graph Expr
foldTreeExpr t = fixTree (tieKnotExpr []) t

foldTreeStack :: Tree Stack -> Graph Stack
foldTreeStack t = fixTree (tieKnotStack []) t

tieKnotExpr :: [Node Expr] -> Node Expr -> Tree Expr -> Graph Expr
tieKnotExpr ns n t@(Node e _) =
	case [(k, r) | k@(Node e' _) <- n:ns, Just r <- [renaming e' e]] of
		[] -> fixTree (tieKnotExpr (n:ns)) t
		(k, r):_ -> Node e (Fold k r)

tieKnotStack :: [Node Stack] -> Node Stack -> Tree Stack -> Graph Stack
tieKnotStack ns n t@(Node e _) =
    case [(k, r) | k@(Node e' _) <- n:ns, Just r <- [renamingStack e' e]] of
        [] -> fixTree (tieKnotStack (n:ns)) t
        (k, r):_ -> Node e (Fold k r)


-- here is a trick with recursive values: t where t = .... t
-- not that fix tree is polymorphic!!!
fixTree :: (Node a -> Tree a -> Graph a) -> Tree a -> Graph a
fixTree f (Node e (Transient c)) = t where
	t = Node e $ Transient $ f t c
fixTree f (Node e (Decompose n cs)) = t where 
	t = Node e $ Decompose n [f t c | c <- cs]
fixTree f (Node e (Variants cs)) = t where
	t = Node e $ Variants [(p, f t c) | (p, c) <- cs]
fixTree f (Node e Stop) = (Node e Stop)