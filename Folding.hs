module Folding(foldTree) where
	
import Driving
import Language
import Data.List
import Data.Maybe

-- folding (without generalization) of an infinite tree into a graph
foldTree :: Tree -> Tree
foldTree t = foldTree1 [] t

foldTree1 :: [Tree] -> Tree -> Tree
foldTree1 ts t@(Node e _) = maybe (fold1 ts t) (\ _ -> Node e Fold) candidate where
	candidate = find (isJust . renaming e . expr) ts
	
fold1 :: [Tree] -> Tree -> Tree
fold1 ts (Node e (TransientStep c)) = 
	let t1 = Node e $ TransientStep $ foldTree1 (t1:ts) c in t1
fold1 ts (Node e (DecomposeStep cs)) =
	let t1 = Node e $ DecomposeStep $ map (foldTree1 (t1:ts)) cs in t1
fold1 ts (Node e (ContractStep cs)) =
	let t1 = Node e $ ContractStep $ map (\(c, t) -> (c, foldTree1 (t1:ts) t)) cs in t1
fold1 ts (Node e Stop) =
	(Node e Stop)

expr (Node e _) = e