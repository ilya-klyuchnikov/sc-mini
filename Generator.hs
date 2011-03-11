module Generator where
	
import Data
import DataUtil

residuate :: Graph Conf -> Task
residuate tree = (expr, program) where
	(expr, program, _) = res nameSupply [] tree

--- generation of residual program
res :: NameSupply -> [(Conf, Conf)] -> Graph Conf -> (Conf, Program, NameSupply)
res ns mp (Node e Stop) = (e, Program [] [], ns)

res ns mp (Node (Ctr cname _) (Decompose ts)) = (Ctr cname args, p1, ns1) where
	(args, p1, ns1) = res' ns mp ts

res ns mp (Node (Let (v, _) _) (Decompose ts)) = (e2 // [(v, e1)], p1, ns1) where
	([e1, e2], p1, ns1) = res' ns mp ts

res (n:ns) mp (Node e (Transient t)) = (fcall, Program ((FDef f1 vs body):fs) gs, ns1) where
	vs = vnames e
	f1 = "ff" ++ (tail n)
	fcall = FCall f1 $ map Var vs
	(body, Program fs gs, ns1) = res ns ((e, fcall) : mp) t
	
res (n:ns) mp (Node e (Variants cs)) = (gcall, Program fs (newGs ++ gs), ns1) where
	vs@(pv:vs') = vnames e
	(vs_, vs'_) = if (isRepeated pv e) && (isUsed pv cs) then (pv:vs, vs) else (vs, vs')
	g1 = "gg" ++ (tail n)
	gcall = GCall g1 $ map Var vs_
	(bodies, Program fs gs, ns1) = res' ns ((e, gcall) : mp) $ map snd cs
	pats = [pat | (Contract v pat, _) <- cs]
	newGs = [GDef g1 p vs'_ b | (p, b) <-  (zip pats bodies)]
	isUsed vname cs = any (any (== vname) . vnames . nodeLabel . snd) cs
	
res ns mp (Node e (Fold (Node base _) ren)) = (call, Program [] [], ns) where
	call = baseCall // [(x, Var y) | (x, y) <- ren] 
	Just baseCall = lookup base mp

-- proceeds a list of trees 
-- the main goal is to handle name supply
res' :: NameSupply -> [(Conf, Conf)] -> [Graph Conf] -> ([Conf], Program, NameSupply)
res' ns mp ts = foldl f ([], Program [] [], ns) ts where 
	f (cs, Program fs gs, ns1) t = (cs ++ [g], Program (fs ++ fs1) (gs ++ gs1), ns2) where 
		(g, Program fs1 gs1, ns2) = res ns1 mp t

isBase e1 (Node _ (Decompose ts)) = or $ map (isBase e1) ts
isBase e1 (Node _ (Variants cs)) = or $ map (isBase e1 . snd) cs 
isBase e1 (Node _ (Transient t)) = isBase e1 t
isBase e1 (Node _ (Fold (Node e2 _) _)) = e1 == e2
isBase e1 (Node e2 Stop) = False