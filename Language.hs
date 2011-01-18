module Language where

import Data.List
import Data.Maybe

data Expr = Var String | Ctr String [Expr] | FCall String [Expr] | GCall String [Expr] | Let String Expr Expr deriving (Show, Eq)
data Pat = Pat String [String] deriving (Show)
data GFun = GFun String Pat [String] Expr deriving (Show)
data FFun = FFun String [String] Expr deriving (Show)
data Program = Program [FFun] [GFun] deriving (Show)
type Subst = [(String, Expr)]
type NameSupply = [String]

subst :: Subst -> Expr -> Expr
subst sub (Var x)  = maybe (Var x) snd (find ((== x) . fst) sub)
subst sub (Ctr name args)  = Ctr name (map (subst sub) args)
subst sub (FCall name args)  = FCall name (map (subst sub) args)
subst sub (GCall name args) = GCall name (map (subst sub) args)
subst sub (Let x e1 e2) = Let x (subst sub e1) (subst sub e2)

fFun :: Program -> String -> FFun
fFun (Program fs _) fname = fromJust (find (\(FFun x _ _) -> x == fname) fs)

gFun :: Program -> String -> String -> GFun
gFun (Program _ gs) gname cname = fromJust (find (\(GFun x (Pat cname1 _) _ _) -> (x == gname) && cname == cname1) gs)

gFuns :: Program -> String -> [GFun]
gFuns (Program _ gs) gname = filter (\(GFun x _ _ _) -> x == gname) gs

rawRenaming :: (Expr, Expr) -> [Maybe (String, String)]
rawRenaming ((Var x), (Var y)) = [Just (x, y)]
rawRenaming ((Ctr n1 args1), (Ctr n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming ((FCall n1 args1), (FCall n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming ((GCall n1 args1), (GCall n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming (Let v e1 e2, Let v' e1' e2') = rawRenaming (e1, e1') ++ rawRenaming (e2, subst [(v, Var v')] e2')
rawRenaming _  = [Nothing]

renaming :: Expr -> Expr -> Maybe[(String, String)]
renaming e1 e2 = f $ partition isNothing $ rawRenaming (e1, e2) where
	f (x:_, _) = Nothing
	f (_, ps) = g $ groupBy (\(a, b) (c, d) -> a == c) $ nub $ catMaybes ps
	g xs = if all ((== 1) . length) xs then Just (concat xs) else Nothing