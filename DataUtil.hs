module DataUtil where
	
import Data
import Data.Maybe
import Data.Char
import Data.List

fDef :: Program -> Name -> FDef
fDef (Program fs _) fname = head [f | f@(FDef x _ _) <- fs, x == fname]

gDefs :: Program -> Name -> [GDef]
gDefs (Program _ gs) gname = [g | g@(GDef x _ _ _) <- gs, x == gname]

gDef :: Program -> Name -> Name -> GDef
gDef p gname cname = head [g | g@(GDef _ (Pat c _) _ _) <- gDefs p gname, c == cname]

(//) :: Expr -> Subst -> Expr
(Var x) // sub = maybe (Var x) id (lookup x sub)
(Ctr name args) // sub = Ctr name (map (// sub) args)
(FCall name args) // sub = FCall name (map (// sub) args)
(GCall name args) // sub = GCall name (map (// sub) args)

renaming :: Expr -> Expr -> Maybe Renaming
renaming e1 e2 = mergeRenaming $ renaming' e1 e2

mergeRenaming :: [Maybe (Variable, Variable)] -> Maybe Renaming
mergeRenaming rens = f $ partition isNothing rens where
	f (x:_, _) = Nothing
	f (_, ps) = g gs1 gs2
		where 
			gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy h $ nub $ catMaybes ps
			gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy h $ nub $ catMaybes ps
			h (a, b) (c, d) = compare a c
	g xs ys = if all ((== 1) . length) xs && all ((== 1) . length) ys 
		then Just (concat xs) else Nothing

-- List of pairs of varialbe for corresponding places
renaming' :: Expr -> Expr -> [Maybe (Variable, Variable)]
renaming' (Var x) (Var y) = [Just (x, y)]
renaming' (Ctr n1 args1) (Ctr n2 args2) | n1 == n2 = concat $ zipWith renaming' args1 args2
renaming' (FCall n1 args1) (FCall n2 args2) | n1 == n2 = concat $ zipWith renaming' args1 args2
renaming' (GCall n1 args1) (GCall n2 args2) | n1 == n2 = concat $ zipWith renaming' args1 args2
renaming' _ _ = [Nothing]

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

isTreeless :: Expr -> Bool
isTreeless (Var _) = True
isTreeless (Ctr _ args) = all isTreeless args
isTreeless (FCall _ args) = all isVar args
isTreeless (GCall _ args) = all isVar args

isTreelessP :: Program -> Bool
isTreelessP (Program fs gs) = 
	all (\(FDef _ _ b) -> isTreeless b && isLinear b) fs && all (\(GDef _ _ _ b) -> isTreeless b && isLinear b) gs

vars :: Expr -> [Variable]
vars (Var v) = [v]
vars (Ctr _ args)   = concat $ map vars args
vars (GCall _ args) = concat $ map vars args
vars (FCall _ args) = concat $ map vars args

isLinear :: Expr -> Bool
isLinear exp = nub (vars exp) == vars exp
