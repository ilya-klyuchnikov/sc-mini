module DataUtil(
	isValue,isCall,isVar,size,
	fDef, gDef, gDefs,
	(//), renaming, vnames,nameSupply,
	nodeLabel,isRepeated,unused
	) where
	
import Data
import Maybe
import Char
import List

isValue :: Expr -> Bool
isValue (Ctr _ args) = and $ map isValue args 
isValue _ = False

isCall :: Expr -> Bool
isCall (FCall _ _) = True
isCall (GCall _ _) = True
isCall _ = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

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
(Let (x, e1) e2) // sub  = Let (x, (e1 // sub)) (e2 // sub)

size :: Expr -> Integer
size (Var _) = 1
size (Ctr _ args) = 1 + sum (map size args)
size (FCall _ args) = 1 + sum (map size args)
size (GCall _ args) = 1 + sum (map size args)
size (Let (_, e1) e2) = 1 + (size e1) + (size e2)

-- global success = no local failures ++ all local successes are the same
renaming :: Expr -> Expr -> Maybe Renaming
renaming e1 e2 = f $ partition isNothing $ rawRenaming (e1, e2) where
	f (x:_, _) = Nothing
	f (_, ps) = g gs1 gs2
		where 
			gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy (\(a, b) (c, d) -> compare a c) $ nub $ catMaybes ps
			gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy (\(a, b) (c, d) -> compare b d) $ nub $ catMaybes ps
	g xs ys = if all ((== 1) . length) xs && all ((== 1) . length) ys then Just (concat xs) else Nothing

rawRenaming :: (Expr, Expr) -> [Maybe (Name, Name)]
rawRenaming ((Var x), (Var y)) = [Just (x, y)]
rawRenaming ((Ctr n1 args1), (Ctr n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming ((FCall n1 args1), (FCall n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming ((GCall n1 args1), (GCall n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming (Let (v, e1) e2, Let (v', e1') e2') = rawRenaming (e1, e1') ++ rawRenaming (e2, e2' // [(v, Var v')])
rawRenaming _  = [Nothing]
	
--isRenaming e1 e2 = isJust $ renaming e1 e2

nameSupply :: NameSupply
nameSupply = ["v" ++ (show i) | i <- [1 ..] ]
nodeLabel (Node l _) = l
step (Node _ s) = s
unused (Contract _ (Pat _ vs)) = (\\ vs)

-- a set (so, with no duplicates) of all variable name int the order 
-- og their first appearence in the expression
vnames :: Expr -> [Name]
vnames = nub . vnames1

-- a LIST (so, with possible duplicates) of all variable names encountered in a given expressions
-- variables are in the order they are encountered
vnames1 :: Expr -> [Name]
vnames1 (Var v) = [v]
vnames1 (Ctr _ args)   = concat $ map vnames1 args
vnames1 (FCall _ args) = concat $ map vnames1 args
vnames1 (GCall _ args) = concat $ map vnames1 args
vnames1 (Let (_, e1) e2) = vnames1 e1 ++ vnames1 e2

isRepeated vn e = (length $ filter (== vn) (vnames1 e)) > 1