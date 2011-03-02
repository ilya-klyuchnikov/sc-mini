module Util where

import Data
import DataUtil

-- Latex
pprintLTree :: Graph Conf -> String
pprintLTree (Node expr next) = make next where
	make (Fold _ _) = "node[conf]{" ++ (show expr) ++ "}"
	make Stop = "node[conf]{" ++ (show expr) ++ "}"
	make (Transient t) = "node[conf]{" ++ (show expr) ++ "}\nchild[->]{" ++ (pprintLTree t) ++ "}"
	make (Decompose ts) = "node[conf]{" ++ (show expr) ++ "}" ++ 
		(concat (map (\t -> "\nchild[->]{" ++ (pprintLTree t) ++ "}") ts))
	make (Variants [(x1, t1), (x2, t2)]) = 
		"node[conf]{" ++ (show expr) ++ "}" ++ 
			("\nchild[->]{" ++ (pprintLTree t1) ++ "\nedge from parent node[left,label,xshift=-5mm]{" ++ (show x1) ++ "}}") ++
			("\nchild[->]{" ++ (pprintLTree t2) ++ "\nedge from parent node[right,label,xshift=5mm]{" ++ (show x2) ++ "}}")
