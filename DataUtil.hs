module DataUtil(
  isValue,isCall,isVar,size,
  fDef, gDef, gDefs,
  (//), renaming, vnames,nameSupply,
  nodeLabel,isRepeated,unused
  ) where

import Data
import Data.Maybe
import Data.Char
import Data.List

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

nameSupply :: NameSupply
nameSupply = ["v" ++ (show i) | i <- [1 ..] ]

unused :: Contract -> NameSupply -> NameSupply
unused (Contract _ (Pat _ vs)) = (\\ vs)

vnames :: Expr -> [Name]
vnames = nub . vnames'

vnames' :: Expr -> [Name]
vnames' (Var v) = [v]
vnames' (Ctr _ args)   = concat $ map vnames' args
vnames' (FCall _ args) = concat $ map vnames' args
vnames' (GCall _ args) = concat $ map vnames' args
vnames' (Let (_, e1) e2) = vnames' e1 ++ vnames' e2

isRepeated :: Name -> Expr -> Bool
isRepeated vn e = (length $ filter (== vn) (vnames' e)) > 1

renaming :: Expr -> Expr -> Maybe Renaming
renaming e1 e2 = f $ partition isNothing $ renaming' (e1, e2) where
  f (x:_, _) = Nothing
  f (_, ps) = g gs1 gs2
    where
      gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy h $ nub $ catMaybes ps
      gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy j $ nub $ catMaybes ps
      h (a, b) (c, d) = compare a c
      j (a, b) (c, d) = compare b d
  g xs ys = if all ((== 1) . length) xs && all ((== 1) . length) ys
    then Just (concat xs) else Nothing

renaming' :: (Expr, Expr) -> [Maybe (Name, Name)]
renaming' ((Var x), (Var y)) = [Just (x, y)]
renaming' ((Ctr n1 args1), (Ctr n2 args2)) | n1 == n2 = concat $ map renaming' $ zip args1 args2
renaming' ((FCall n1 args1), (FCall n2 args2)) | n1 == n2 = concat $ map renaming' $ zip args1 args2
renaming' ((GCall n1 args1), (GCall n2 args2)) | n1 == n2 = concat $ map renaming' $ zip args1 args2
renaming' (Let (v, e1) e2, Let (v', e1') e2') = renaming' (e1, e1') ++ renaming' (e2, e2' // [(v, Var v')])
renaming' _  = [Nothing]

size :: Expr -> Integer
size (Var _) = 1
size (Ctr _ args) = 1 + sum (map size args)
size (FCall _ args) = 1 + sum (map size args)
size (GCall _ args) = 1 + sum (map size args)
size (Let (_, e1) e2) = 1 + (size e1) + (size e2)

nodeLabel :: Node a -> a
nodeLabel (Node l _) = l

step :: Node a -> Step (Graph a)
step (Node _ s) = s
