module Data where
-- This module contains definitions of data structures and basic utility functions

import Data.List
import Data.Maybe

data Contract = Contract Name Pat deriving (Show)
type Variant a = (Contract, a)
data Step a = Transient a | Decompose [a] | Variants [(Contract, a)] | Stop | Fold a Renaming deriving (Show)
data Tree = Node Expr (Step Tree) deriving (Show)
data Expr = Var Name | Ctr Name [Expr] | FCall Name [Expr] | GCall Name [Expr] | Let Name Expr Expr deriving (Show, Eq)
data Pat = Pat Name [Name] deriving (Show)
data GFun = GFun Name Pat [Name] Expr deriving (Show)
data FFun = FFun Name [Name] Expr deriving (Show)
data Program = Program [FFun] [GFun] deriving (Show)
type Subst = [(Name, Expr)]
type NameSupply = [Name]
type Name = String
type Renaming = [(Name, Name)]

nameSupply = ["$" ++ (show i) | i <- [1 ..] ]
expr (Node e _) = e
step (Node _ s) = s
unused (Contract _ (Pat _ vs)) = drop (length vs)

fFun :: Program -> Name -> FFun
fFun (Program fs _) fname = head [f | f@(FFun x _ _) <- fs, x == fname]

gFuns :: Program -> Name -> [GFun]
gFuns (Program _ gs) gname = [g | g@(GFun x _ _ _) <- gs, x == gname]

gFun :: Program -> Name -> Name -> GFun
gFun p gname cname = head [g | g@(GFun _ (Pat c _) _ _) <- gFuns p gname, c == cname]

subst :: Subst -> Expr -> Expr
subst sub (Var x)  = maybe (Var x) id (lookup x sub)
subst sub (Ctr name args)  = Ctr name (map (subst sub) args)
subst sub (FCall name args)  = FCall name (map (subst sub) args)
subst sub (GCall name args) = GCall name (map (subst sub) args)
subst sub (Let x e1 e2) = Let x (subst sub e1) (subst sub e2)

rawRenaming :: (Expr, Expr) -> [Maybe (Name, Name)]
rawRenaming ((Var x), (Var y)) = [Just (x, y)]
rawRenaming ((Ctr n1 args1), (Ctr n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming ((FCall n1 args1), (FCall n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming ((GCall n1 args1), (GCall n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming (Let v e1 e2, Let v' e1' e2') = rawRenaming (e1, e1') ++ rawRenaming (e2, subst [(v, Var v')] e2')
rawRenaming _  = [Nothing]

renaming :: Expr -> Expr -> Maybe[(Name, Name)]
renaming e1 e2 = f $ partition isNothing $ rawRenaming (e1, e2) where
	f (x:_, _) = Nothing
	f (_, ps) = g $ groupBy (\(a, b) (c, d) -> a == c) $ nub $ catMaybes ps
	g xs = if all ((== 1) . length) xs then Just (concat xs) else Nothing
	
isRenaming e1 e2 = isJust $ renaming e1 e2

size :: Expr -> Integer
size (Var _) = 1
size (Ctr _ args) = 1 + sum (map size args)
size (FCall _ args) = 1 + sum (map size args)
size (GCall _ args) = 1 + sum (map size args)
size (Let _ e1 e2) = 1 + (size e1) + (size e2)

vnames :: Expr -> [Name]
vnames (Var v) = [v]
vnames (Ctr _ args)   = nub $ concat $ map vnames args
vnames (FCall _ args) = nub $ concat $ map vnames args
vnames (GCall _ args) = nub $ concat $ map vnames args