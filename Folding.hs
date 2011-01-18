module Folding(foldTree) where
	
import Driving
import Language
import Data.List
import Data.Maybe

-- folding (without generalization) of an infinite tree into a graph
foldTree :: Tree -> Tree
foldTree t = foldTree1 [] t

foldTree1 :: [Tree] -> Tree -> Tree
foldTree1 ts t@(Node e _) = 
	if any (isRenaming e . expr) ts then Node e Fold else fold1 ts t
	
fold1 :: [Tree] -> Tree -> Tree
fold1 ts (Node e (Transient c)) = t where
	t = Node e $ Transient $ foldTree1 (t:ts) c
fold1 ts (Node e (Decompose cs)) = t where 
	t = Node e $ Decompose [foldTree1 (t:ts) c | c <- cs]
fold1 ts (Node e (Variants cs)) = t where
	t = Node e $ Variants [(c, foldTree1 (t:ts) v) | (c, v) <- cs]
fold1 ts (Node e Stop) = (Node e Stop)

expr (Node e _) = e