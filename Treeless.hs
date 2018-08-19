import Data
import Data.List

isTreelessProgram :: Program -> Bool
isTreelessProgram (Program fs gs) =
  all (\(FDef _ _ b)   -> isTreelessExpr b && isLinearExpr b) fs &&
  all (\(GDef _ _ _ b) -> isTreelessExpr b && isLinearExpr b) gs

isTreelessExpr :: Expr -> Bool
isTreelessExpr (Var _) = True
isTreelessExpr (Ctr _ args) = all isTreelessExpr args
isTreelessExpr (FCall _ args) = all isVar args
isTreelessExpr (GCall _ args) = all isVar args

isLinearExpr :: Expr -> Bool
isLinearExpr exp = nub (vars exp) == vars exp

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

-- varibles in order - including duplicates
vars :: Expr -> [Name]
vars (Var v) = [v]
vars (Ctr _ args)   = concat $ map vars args
vars (GCall _ args) = concat $ map vars args
vars (FCall _ args) = concat $ map vars args
