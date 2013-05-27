module Experiment1Sll where

import Data
import DataUtil
import DataIO
import Driving
import Interpreter
import TreeInterpreter
import Folding
import Deforester
import Stack

-- deforestator
dprog :: Program
dprog = read
	" fProg() = Program( \
 	\ Cons( \
    \   FDef(DAppend(), Cons(L1(), Cons(L2(), Cons(L3(), Nil()))), GCall(Append(), Cons(GCall(Append(), Cons(L1(), Cons(L2(), Nil()))), Cons(L3(), Nil())))), \
  	\ Cons( \
    \   FDef(TAppend(), Cons(L1(), Cons(L2(), Cons(L3(), Cons(L4(), Nil())))), GCall(Append(), Cons(GCall(Append(), Cons(L1(), Cons(L2(), Nil()))), Cons(GCall(Append(), Cons(L3(), Cons(L3(), Nil()))),  Nil())))), \ 
  	\ Nil())), \ 
  	\ Nil()); \
	\ \
	\ gFDef(Program(fs, gs), fname) = gFDef1(fname, fs); \
	\ gFDef1(DAppend(), fs) = gFDef1DAppend(fs); \
	\ gFDef1(TAppend(), fs) = gFDef1TAppend(fs); \
	\ gFDef1DAppend(Cons(f, fs)) = gFDef2DAppend(f, fs); \
	\ gFDef1TAppend(Cons(f, fs)) = gFDef2TAppend(f, fs); \
	\ gFDef2DAppend(FDef(fname, args, body), fs) = gFDef3DAppend(fname, args, body, fs); \
	\ gFDef2TAppend(FDef(fname, args, body), fs) = gFDef3TAppend(fname, args, body, fs); \
	\ gFDef3DAppend(DAppend(), args, body, fs) = FDef(DAppend(), args, body); \
	\ gFDef3DAppend(TAppend(), args, body, fs) = gFDef1DAppend(fs); \
	\ gFDef3TAppend(TAppend(), args, body, fs) = FDef(TAppend(), args, body); \
	\ gFDef3TAppend(DAppend(), args, body, fs) = gFDef1TAppend(fs); "

prog :: Program
prog = read
    " gApp(Cons(l1, l2), l3) = Cons(l1, gApp(l2, l3)); \
    \ gApp(Nil(), l1) = l1; \
    \ fDapp(l1, l2, l3) = gApp(gApp(l1, l2), l3); \
    \ fTapp(l1, l2, l3) = gApp(gApp(l1, l2), gApp(l3, l4)); "

-- serialize program into sll
quoteP :: Program -> Expr
quoteP (Program fs gs) = 
	Ctr "Program" [list $ map quoteF fs, list $ map quoteF fs]

quoteF (FDef (_:n) vs b) = 
    Ctr "FDef" [Ctr n [], list $ map quoteV vs, quoteE b]
quoteG (GDef (_:n) (Pat pn pvs) vs b) =
    Ctr "GDef" [Ctr n [], Ctr "Pat" [Ctr pn [], list $ map quoteV pvs], list $ map quoteV vs, quoteE b]

list :: [Expr] -> Expr
list []     = Ctr "Nil"  []
list (x:xs) = Ctr "Cons" [x, list xs]

quoteV :: Variable -> Expr
quoteV (NVar n) = Ctr "LVar" [Ctr n []]
-- one level of quoting
quoteE :: Expr -> Expr
quoteE (Var (NVar n))      = Ctr "Var" [Ctr "LVar" [Ctr n []]] -- "Var(LVar(L1()))"
quoteE (Ctr n args)        = Ctr "Ctr" [Ctr n [], quoteA args]
quoteE (FCall (_:n) args)  = Ctr "FCall" [Ctr n [], quoteA args]
quoteE (GCall (_:n) args)  = Ctr "GCall" [Ctr n [], quoteA args]

quoteA args = list $ map quoteE args

x = quoteE $ read "l1"
y = quoteP prog