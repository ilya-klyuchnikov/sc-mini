module Experiment1 where

-- hand coded deforester for a simple program
-- app(Cons(l1, l2), l3) = Cons(l1, app(l2, l3));
-- app(Nil(), l1) = l1;

-- global vars - free variables
data GVName = G1 | G2 | G3
-- local vars - vars in function
data LVName = L1 | L2 | L3
data CName = Cons | Nil
data GName = Append
data FName -- no constructors
data Variable = GVar GVName | LVar LVName
-- selectors
data Sel = Head | Tail
---

data Expr = Var Variable | Ctr CName [Expr] | FCall FName [Expr] | GCall GName [Expr]
data Pat = Pat CName [Variable]
data GDef = GDef GName Pat [Variable] Expr
data FDef = FDef FName [Variable] Expr
data Program = Program [FDef] [GDef]
