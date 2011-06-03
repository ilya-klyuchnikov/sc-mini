module Demonstration where

import Data
import DataUtil
import DataIO
import Driving
import Interpreter
import TreeInterpreter
import Supercompiler
import Folding
import Data.List
import Data.Maybe
import Generator
import Prototype
import Deforester
import NeighborhoodAnalysis

prog1 :: Program
prog1 = read
	" gAdd(Z(), y) = y;\
	\ gAdd(S(x), y) = S(gAdd(x, y));\
	\ gMult(Z(), y) = Z();\
	\ gMult(S(x), y) = gAdd(y, gMult(x, y));\ 
	\ fSqr(x) = gMult(x, x); \
	\ gEven(Z()) = True();\
	\ gEven(S(x)) = gOdd(x);\
	\ gOdd(Z()) = False();\
	\ gOdd(S(x)) = gEven(x);\
	\ gAdd1(Z(), y) = y; \
	\ gAdd1(S(x), y) = gAdd1(x, S(y)); \
	
	\ gLt(Z(), y)  = gLt1(y);\
	\ gLt(S(x), y) = gLt2(y, x);\
	
	\ gLt1(Z()) = False();\
	\ gLt1(S(x)) = True();\
	
	\ gLt2(Z(), x) = False();\
	\ gLt2(S(y), x) = gLt(x, y);\
	
	\ fTest(x, y, z) = P(gLt(x, z), gLt(y, z));"

prog2 :: Program
prog2 = read
	" gEqSymb(A(), y) = gEqA(y);\
	\ gEqSymb(B(), y) = gEqB(y);\
	\ gEqA(A()) = True();  gEqA(B()) = False();\
	\ gEqB(A()) = False(); gEqB(B()) = True();\
	\ gIf(True(), x, y) = x;\
	\ gIf(False(), x, y) = y;\
	\ fMatch(p, s) = gM(p, s, p, s);\
	\ gM(Nil(), ss, op, os) = True();\
	\ gM(Cons(p, pp), ss, op, os) = gX(ss, p, pp, op, os);\
	\ gX(Nil(), p, pp,  op, os) = False();\
	\ gX(Cons(s, ss), p, pp,  op, os) = gIf(gEqSymb(p, s), gM(pp, ss, op, os), gN(os, op));\
	\ gN(Nil(), op) = False(); \
	\ gN(Cons(s, ss), op) = gM(op, ss, op, ss);"
	
prog3 :: Program
prog3 = read
	" gAdd(Z(), y) = y;\
	\ gAdd(S(x), y) = S(gAdd(x, y));\
	\ gDouble(Z()) = Z(); \
	\ gDouble(S(x)) = S(S(gDouble(x))); \
	\ gHalf(Z()) = Z(); \
	\ gHalf(S(x)) = gHalf1(x); \ 
	\ gHalf1(Z()) = Z(); \
	\ gHalf1(S(x)) = S(gHalf(x)); \
	\ gEq(Z(), y) = gEqZ(y); \
	\ gEq(S(x), y) = gEqS(y, x); \
	\ gEqZ(Z()) = True(); \
	\ gEqZ(S(x)) = False(); \
	\ gEqS(Z(), x) = False(); \
	\ gEqS(S(y), x) = gEq(x, y);"
	
	
prog4 :: Program
prog4 = read
	" fInf() = S(fInf()); \
	\ fB(x) = fB(S(x));"

-- counting steps of interpreter
demo01 = 
	intC prog1 $ read "gEven(fSqr(S(S(Z()))))"

-- int and eval produce the same values
demo02 =
	int prog1 $ read "gEven(fSqr(S(S(Z()))))"
demo03 =
	eval prog1 $ read "gEven(fSqr(S(S(Z()))))"
demo04 =
	int prog1 $ read "fSqr(S(S(Z())))"
demo05 =
	eval prog1 $ read "fSqr(S(S(Z())))"

-- trying interpret undefined expression	
demo06 =
	int  prog1 $ read "fSqr(S(S(x)))"

-- trying eval undefined expression
demo07 =
	eval prog1 $ read "fSqr(S(S(x)))"

-- "interpret" infinite number	
demo08 =
	int  prog4 $ read "fInf()"

-- "eval" infinite number	
demo09 =
	eval prog4 $ read "fInf()"

-- 	driving (variants)
demo10 =
	(driveMachine prog1) nameSupply (read "gOdd(gAdd(x, gMult(x, S(x))))")

-- driving (transient step)	
demo11 = 
	(driveMachine prog1) nameSupply (read "gOdd(S(gAdd(v1, gMult(x, S(x)))))")
	
-- building infinite tree
demo12 =
	putStrLn $ printTree $ buildTree (driveMachine prog1) (read "gEven(fSqr(x))")

-- using intTree (infinite tree) to run task 
demo13 =
	intTree (buildTree (driveMachine prog1) (read "gEven(fSqr(x))")) [("x", read "S(S(Z()))")]
	
-- using intTree (folded finite graph) to run task
demo13a = 
	intTree (foldTree $ buildTree (driveMachine prog1) (read "gEven(fSqr(x))")) [("x", read "S(S(Z()))")]

-- using intTree (infinite tree) to run task
demo14 =
	intTree (buildTree (driveMachine prog1) (read "gEven(fSqr(x))")) [("x", read "S(S(S(Z())))")]

-- successful folding
demo15 =
	putStrLn $ printTree $ foldTree $ buildTree (driveMachine prog1) (read "gEven(fSqr(x))")
	
-- successful folding (tex)
demo15a =
	putStrLn $ pprintLTree $ foldTree $ buildTree (driveMachine prog1) (read "gEven(fSqr(x))")

-- an example of "not foldable" tree
demo16 =
	putStrLn $ printTree $ foldTree $ buildTree (driveMachine prog1) (read "gAdd1(x, y)")

-- an example of generalization, set sizeBound = 5 to get the same result as in the paper
demo17 =
	putStrLn $ printTree $ foldTree $ buildFTree (driveMachine prog1) (read "gAdd1(x, y)")
	
-- even/sqr - just transformation
demo18 = do
	let (c2, p2) = transform ((read "gEven(fSqr(x))"), prog1)
	putStrLn "\ntransformation:\n"
	putStrLn (show c2)
	putStrLn (show p2)
	
-- even/sqr - deforestation
demo19 = do
	let (c2, p2) = deforest ((read "gEven(fSqr(x))"), prog1)
	putStrLn "\ndeforestation:\n"
	putStrLn (show c2)
	putStrLn (show p2)

-- even/sqr - supercompilation
demo20 = do
	let (c2, p2) = supercompile ((read "gEven(fSqr(x))"), prog1)
	putStrLn "supercompilation:\n"
	putStrLn (show c2)
	putStrLn (show p2)

-- KMP -- transform -- graph
demo21 = 
	putStrLn $ printTree $ foldTree $ buildFTree (driveMachine prog2) conf2

-- KMP -- deforest -- graph
demo22 = 
	putStrLn $ printTree $ simplify $ foldTree $ buildFTree (driveMachine prog2) conf2

-- KMP -- supercompile -- graph
demo23 = 
	putStrLn $ printTree $ simplify $ foldTree $ buildFTree (addPropagation (driveMachine prog2)) conf2

-- KMP -- supercompile -- graph
demo23Tex = 
	putStrLn $ pprintLTree $ simplify $ foldTree $ buildFTree (addPropagation (driveMachine prog2)) conf2

g = simplify $ foldTree $ buildFTree (addPropagation (driveMachine prog2)) conf2

demo24 = do
	let (c2, p2) = residuate g
	putStrLn (show c2)
	putStrLn (show p2)

-- KMP - transformation
demo25 = do
	let (c2, p2) = transform (conf2, prog2)
	putStrLn (show c2)
	putStrLn (show p2)
	
-- KMP - deforestation
demo26 = do
	let (c2, p2) = deforest (conf2, prog2)
	putStrLn (show c2)
	putStrLn (show p2)

-- KMP - supercompilation
demo27 = do
	let (c2, p2) = supercompile (conf2, prog2)
	putStrLn (show c2)
	putStrLn (show p2)

-- "program analysis"
demo30 = do
	let (c2, p2) = supercompile (read "gAdd(gAdd(x, y), z)", prog1)
	putStrLn (show c2)
	putStrLn (show p2)
	
demo31 = do
	let (c2, p2) = supercompile (read "gAdd(x, gAdd(y, z))", prog1)
	putStrLn (show c2)
	putStrLn (show p2)

-- supercompiled eqpressions are equal => 
-- original expressions are equivalent	
demo32 =
	supercompile (read "gAdd(x, gAdd(y, z))", prog1) == supercompile (read "gAdd(gAdd(x, y), z)", prog1)

demo33 = do
	let (c2, p2) = supercompile ((read "gEq(gHalf(gDouble(n)),n)"), prog3)
	putStrLn "supercompilation:\n"
	putStrLn (show c2)
	putStrLn (show p2)
	

-- all further stuff is for "benchmarking"
-- set sizeBound=10 to get the same results as in the paper
conf1 :: Expr
conf1 = read "gEven(fSqr(x))"
conf2 :: Expr
conf2 = read "fMatch(Cons(A(), Cons(A(), Cons(B(), Nil()))), s)"

-- input task
t1 = (conf1, prog1)
-- transformed task
t1t = transform t1
-- deforested task
t1d = deforest t1
-- supercompiled task
t1s = supercompile t1


run st n = sll_trace st [("x", peano n)]

def (e, p) = simplify $ foldTree $ buildFTree (driveMachine p) e
tr (e, p) = foldTree $ buildFTree (driveMachine p) e

t1d' = def t1
t1t' = tr t1

peano 0 = Ctr "Z" []
peano n = Ctr "S" [peano (n - 1)]


benchmark0 = map (snd . (run t1)) [0 .. 50]
benchmark1 = map (snd . (run t1t)) [0 .. 50]
benchmark2 = map (snd . (run t1d)) [0 .. 50]
benchmark3 = map (snd . (run t1s)) [0 .. 50]

points1 = zipWith3 (\n x1 x2 -> (n, (fromInteger x1) / (fromInteger x2))) [0 .. 50] benchmark0 benchmark1
points2 = zipWith3 (\n x1 x2 -> (n, (fromInteger x1) / (fromInteger x2))) [0 .. 50] benchmark0 benchmark2
points3 = zipWith3 (\n x1 x2 -> (n, (fromInteger x1) / (fromInteger x2))) [0 .. 50] benchmark0 benchmark3

testTree1 =
	putStrLn $ printTree $ buildTree (driveMachine prog2) (read "fMatch(Cons(A(), Nil()), Cons(A(), Nil()))")

--- NeighborhoodAnalysis
data1 :: Conf
data1 = (read "fMatch(Cons(A(), Nil()), Cons(A(), Cons(A(), Nil())))")
data1S :: Conf
data1S = (read "fMatch(x, y)")

testNan1 = nan (addPropagation $ driveMachine prog2) data1 data1S

data2 :: Conf
data2 = (read "gEqSymb(A(), A())")

data2S :: Conf
data2S = (read "gEqSymb(y, x)")

testNan2 = nan (addPropagation $ driveMachine prog2) data2 data2S

data3 :: Conf
data3 = (read "fTest(Z(), S(Z()), S(S(Z())))")

-- x < z
-- y < z
data3S :: Conf
data3S = (read "fTest(x, y, z)")
testNan3 = nan (addPropagation $ driveMachine prog1) data3 data3S

-------------

main = do
	putStrLn "demo01"
	putStrLn (show demo01)
	
	putStrLn "\ndemo02"
	putStrLn (show demo02)
	
	putStrLn "\ndemo03"
	putStrLn (show demo03)
	
	putStrLn "\ndemo04"
	putStrLn (show demo04)
	
	putStrLn "\ndemo05"
	putStrLn (show demo05)
	
	putStrLn "\ndemo06"
	-- error
	--putStrLn (show demo06)
	
	putStrLn "\ndemo07"
	-- error
	--putStrLn (show demo07)
	
	putStrLn "\ndemo08"
	-- bottom
	--putStrLn (show demo08)
	
	putStrLn "\ndemo09"
	-- infinite
	--putStrLn (show demo09)
	
	putStrLn "\ndemo10"
	putStrLn (show demo10)
	
	putStrLn "\ndemo11"
	putStrLn (show demo11)
	
	putStrLn "\ndemo12"
	demo12
	
	putStrLn "\ndemo13"
	putStrLn (show demo13)
	
	putStrLn "\ndemo14"
	putStrLn (show demo14)
	
	putStrLn "\ndemo15"
	demo15
	
	putStrLn "\ndemo16"
	demo16
	
	putStrLn "\ndemo17"
	demo17
	
	putStrLn "\ndemo18"
	demo18
	
	putStrLn "\ndemo19"
	demo19
	
	putStrLn "\ndemo20"
	demo20
	
	putStrLn "\ndemo21"
	demo21
	
	putStrLn "\ndemo22"
	demo22
	
	putStrLn "\ndemo23"
	demo23
	
	putStrLn "\ndemo24"
	demo24
	
	putStrLn "\ndemo25"
	demo25
	
	putStrLn "\ndemo26"
	demo26
	
	putStrLn "\ndemo27"
	demo27
	
	putStrLn "\ndemo30"
	demo30
	
	putStrLn "\ndemo31"
	demo31
	
	putStrLn "\ndemo32"
	putStrLn (show demo32)
	
	putStrLn "\ndemo33"
	demo33