module Generator where
	
import Language
import Driving
import Data.Maybe
import Data.List

-- simplifies tree - removes transient edges
s :: Tree -> Tree
s (Node e (Decompose ts)) = 
	(Node e (Decompose $ map s ts))

s (Node e (Variants cs)) = 
	Node e $ Variants [(c, s t) | (c, t) <- cs]

s (Node e (Transient t@(Node e1 step))) | isBase e t = 
	if isBase e1 t then Node e $ Transient $ s t else Node e step1 where
		Node _ step1 = s t

s (Node e (Transient t)) = 
	s t

s t = t

isBase e1 (Node _ (Decompose ts)) = or $ map (isBase e1) ts
isBase e1 (Node _ (Variants cs)) = or $ map (isBase e1 . snd) cs 
isBase e1 (Node _ (Transient t)) = isBase e1 t
isBase e1 (Node e2 Fold) = isJust $ renaming e2 e1
isBase e1 (Node e2 Stop) = False

--- generation of residual program
res :: NameSupply -> [(Expr, Expr)] -> Tree -> (Expr, Program, NameSupply)
res ns mp (Node e Stop) = (e, Program [] [], ns)

res ns mp (Node (Ctr cname _) (Decompose ts)) = (Ctr cname args, p1, ns1) where
	(args, p1, ns1) = make ns mp ts

res ns mp (Node (Let v _ _) (Decompose ts)) = (Let v e1 e2, p1, ns1) where
	([e1, e2], p1, ns1) = make ns mp ts

res (n:ns) mp (Node e (Transient t)) = (fcall, Program ((FFun f1 vs body):fs) gs, ns1) where
	vs = vnames e
	f1 = "f" ++ n
	fcall = FCall f1 $ map Var vs
	(body, Program fs gs, ns1) = res ns ((e, fcall) : mp) t
	
res (n:ns) mp (Node e (Variants cs)) = (gcall, Program fs (newGs ++ gs), ns1) where
	vs@(pv:vs') = vnames e
	g1 = "g" ++ n
	gcall = GCall g1 $ map Var vs
	(bodies, Program fs gs, ns1) = make ns ((e, gcall) : mp) $ map snd cs
	pats = [pat | (Contract v pat, _) <- cs]
	newGs = [GFun g1 p vs' b | (p, b) <-  (zip pats bodies)]
	
res ns mp (Node e Fold) = (call, Program [] [], ns) where
	call = subst [(x, Var y) | (x, y) <- ren] baseCall
	(ren, baseCall):_ = catMaybes [fmap (\ren -> (ren, bcall)) (renaming was e) | (was, bcall) <- mp]

make :: NameSupply -> [(Expr, Expr)] -> [Tree] -> ([Expr], Program, NameSupply)
make ns mp ts = foldl f ([], Program [] [], ns) ts where 
	f (gens, Program fs gs, ns1) tree = (gens ++ [g], Program (fs ++ fs1) (gs ++ gs1), ns2) where 
		(g, Program fs1 gs1, ns2) = res ns1 mp tree
		
vnames :: Expr -> [String]
vnames (Var v) = [v]
vnames (Ctr _ args)   = nub $ concat $ map vnames args
vnames (FCall _ args) = nub $ concat $ map vnames args
vnames (GCall _ args) = nub $ concat $ map vnames args
