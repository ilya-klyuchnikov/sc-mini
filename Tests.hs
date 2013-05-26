module Tests where

import Data
import DataUtil
import DataIO
import Driving
import Interpreter
import TreeInterpreter
import Folding
import Deforester
import Stack

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
	(NVar "x",  read "A()"),
	(NVar "xs", read "Cons(A(), Cons(B(), Nil()))"), 
	(NVar "ys", read "Cons(C(), Cons(D(), Nil()))"),
	(NVar "zs", read "Cons(E(), Cons(F(), Nil()))")
	]

inputs :: [Expr]
inputs = map read 
	[
	"gApp(xs, ys)",
	"Cons(x, gApp(xs, ys))", 
	"gApp(gApp(xs, ys), zs)", 
	"gApp1(xs, ys)", 
	"gApp1(gApp1(xs, ys), zs)"
	]

expInputs :: [Expr]
expInputs = map read ["fZeros()", "gApp(fZeros(), fZeros())"]

graph  = foldTreeExpr . buildTree (driveExpr prog)
graph' = simplify . graph
sgraph = foldTreeStack . buildTree (driveStack prog) . mkStack

graphs  = map graph  inputs
graphs' = map graph' inputs
sgraphs = map sgraph inputs

demo :: Show a => Graph a -> IO ()
demo = putStrLn . printTree

demos  = map demo graphs
demos' = map demo graphs'
sdemos = map demo sgraphs

expDemos  = map (demo . graph ) expInputs 
expDemos' = map (demo . graph') expInputs

tests = test (
	["test" ~: "transform" ~: eval prog (i // subst) ~=? intTree (graph i) subst | i <- inputs] ++
	["test" ~: "deforest" ~: eval prog (i // subst) ~=? intTree (graph' i) subst | i <- inputs])
