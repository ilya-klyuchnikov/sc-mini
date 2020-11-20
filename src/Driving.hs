module Driving where

import Data
import DataUtil

buildTree :: Machine Conf -> Conf -> Tree Conf
buildTree m e = bt m nameSupply e

bt :: Machine Conf -> NameSupply -> Conf -> Tree Conf
bt m ns c = case m ns c of
  Decompose ds -> Node c $ Decompose (map (bt m ns) ds)
  Transient e -> Node c $ Transient (bt m ns e)
  Stop -> Node c Stop
  Variants cs -> Node c $ Variants [(c, bt m (unused c ns) e) | (c, e) <- cs]

driveMachine :: Program -> Machine Conf
driveMachine p = drive where
  drive :: Machine Conf
  drive ns (Var _) = Stop
  drive ns (Ctr _ []) = Stop
  drive ns (Ctr _ args) = Decompose args
  drive ns (Let (x, t1) t2) = Decompose [t1, t2]
  drive ns (FCall name args) = Transient $ e // (zip vs args) where
    FDef _ vs e = fDef p name
  drive ns (GCall gn (Ctr cn cargs : args)) = Transient $ e // sub where
    (GDef _ (Pat _ cvs) vs e) = gDef p gn cn
    sub = zip (cvs ++ vs) (cargs ++ args)
  drive ns (GCall gn args@((Var _):_)) = Variants $ variants gn args where
    variants gn args = map (scrutinize ns args) (gDefs p gn)
  drive ns (GCall gn (inner:args)) = inject (drive ns inner) where
    inject (Transient t) = Transient (GCall gn (t:args))
    inject (Variants cs) = Variants $ map f cs
    f (c, t) = (c, GCall gn (t:args))

scrutinize :: NameSupply -> [Expr] -> GDef -> (Contract, Expr)
scrutinize ns (Var v : args) (GDef _ (Pat cn cvs) vs body) =
  (Contract v (Pat cn fresh), body // sub) where
    fresh = take (length cvs) ns
    sub = zip (cvs ++ vs) (map Var fresh ++ args)
