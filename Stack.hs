module Stack where

import Data
import DataUtil

type Stack = [Expr]

mkStack :: Expr -> Stack
mkStack (GCall g (sel:args)) = (mkStack sel) ++ [GCall g (sv:args)]
mkStack e = [e]

mkExpr :: Stack -> Expr
mkExpr [e]            = e
mkExpr (sel : g : s1) = g // [(NVar "_", sel)] 

driveStack :: Program -> Driving Stack
driveStack p (Var _ : []) = 
    Stop
driveStack p (Ctr name [] : []) =
    Stop
driveStack p (Ctr name args : []) = 
    Decompose name $ map mkStack args
driveStack p (FCall name args : s1) = 
    Transient (mkStack e1 ++ s1) where
        e1 = body // zip vs args
        (FDef _ vs body) = fDef p name
driveStack p (Var v : GCall gn (_ : args) : s1) = 
    Variants ss where
        ss = [(contr, mkStack e ++ s1) | (contr, e) <- (map (scrutinize v args) (gDefs p gn))]
driveStack p (Ctr cn cargs : GCall gn (_ : args) : s1) = 
    Transient (mkStack e1 ++ s1) where
        e1 =  body // sub
        (GDef _ pat@(Pat _ cvs) vs body) = gDef p gn cn
        sub = zip (cvs ++ vs) (cargs ++ args)

scrutinize :: Variable -> [Expr] -> GDef -> (Contraction, Expr)
scrutinize v args (GDef _ (Pat cn cvs) vs body) = 
    (Contraction v (Pat cn fresh), body // sub) where
        ids = take (length cvs) $ iterate (+1) 1
        fresh = map (\x -> SVar (cn ++ "_" ++ (show x)) v) ids
        sub = zip (cvs ++ vs) (map Var fresh ++ args)