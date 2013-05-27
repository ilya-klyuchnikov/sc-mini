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

