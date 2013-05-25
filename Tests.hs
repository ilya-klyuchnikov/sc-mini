module Tests where

import Data
import DataUtil
import DataIO
import Driving
import Interpreter
import TreeInterpreter
import Folding
import Deforester

import Test.HUnit

prog :: Program
prog = read
	" gApp(Cons(x, xs), ys) = Cons(x, gApp(xs, ys));\
	\ gApp(Nil(), ys) = ys; \
	\ gApp1(Cons(x, xs), ys) = Cons(x, gApp2(xs, ys));\
	\ gApp1(Nil(), ys) = ys;\
 	\ gApp2(Cons(x, xs), ys) = Cons(x, gApp1(xs, ys));\
	\ gApp2(Nil(), ys) = ys; \
	\ fZeros()  = Cons(Z(), fZeros()); \
	\  \
	\  \
	\  \
	\  \
	\  "


subst :: Subst
subst = [
	(NVar "x", read "Cons(A(), Cons(B(), Nil()))"), 
	(NVar "y", read "Cons(C(), Cons(D(), Nil()))"),
	(NVar "z", read "Cons(E(), Cons(F(), Nil()))")
	]

inputs :: [Expr]
inputs = map read ["gApp(x, y)", "gApp(gApp(x, y), z)", "gApp1(x, y)", "gApp1(gApp1(x, y), z)"]

expInputs :: [Expr]
expInputs = map read ["fZeros()", "gApp(fZeros(), fZeros())"]

graph  = foldTree . buildTree (drive prog)
graph' = simplify . graph

graphs  = map graph  (inputs)
graphs' = map graph' (inputs)

demo = putStrLn . printTree

demos  = map demo graphs
demos' = map demo graphs'

expDemos  = map (demo . graph ) expInputs 
expDemos' = map (demo . graph') expInputs

tests = test (
	["test" ~: "transform" ~: int prog (i // subst) ~=? intTree (graph i) subst | i <- inputs] ++
	["test" ~: "deforest" ~: int prog (i // subst) ~=? intTree (graph' i) subst | i <- inputs])
