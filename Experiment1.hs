module Experiment1 where

-- hand coded deforester for a simple program
-- gApp(Cons(l1, l2), l3) = Cons(l1, gApp(l2, l3));
-- gApp(Nil(), l1) = l1;
-- fDapp(l1, l2, l3) = gApp(gApp(l1, l2), l3);
-- fTapp(l1, l2, l3) = gApp(gApp(l1, l2), gApp(l3, l4));


-- global vars - free variables
data GVName = G1 | G2 | G3
-- local vars - vars in function
data LVName = L1 | L2 | L3 | L4
data CName = Cons1 | Nil1
data GName = Append
-- Double append
data FName = DAppend | TAppend
data Variable = GVar GVName | LVar LVName
-- selectors
data Sel = Head | Tail
---
data List a = Cons a (List a) | Nil
data Expr = Var Variable | Ctr CName (List Expr) | FCall FName (List Expr) | GCall GName (List Expr)
data Pat = Pat CName (List Variable)
data GDef = GDef GName Pat (List Variable) Expr
data FDef = FDef FName (List Variable) Expr
data Program = Program (List FDef) (List GDef)

fDef :: Program -> FName -> FDef
fDef (Program fs gs) fname = fDef1 fname fs

-- When adding new fname, you need to add 
fDef1 DAppend fs = fDef1DAppend fs
fDef1 TAppend fs = fDef1TAppend fs

fDef1DAppend (Cons f fs) = fDef2DAppend f fs
fDef1TAppend (Cons f fs) = fDef2TAppend f fs

fDef2DAppend (FDef fname args body) fs = fDef3DAppend fname args body fs
fDef2TAppend (FDef fname args body) fs = fDef3TAppend fname args body fs

fDef3DAppend DAppend args body fs = FDef DAppend args body
fDef3DAppend TAppend args body fs = fDef1DAppend fs

fDef3TAppend TAppend args body fs = FDef TAppend args body
fDef3TAppend DAppend args body fs = fDef1TAppend fs