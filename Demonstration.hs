module Demonstration where

import Data
import DataUtil
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
	\ gAdd1(S(x), y) = gAdd1(x, S(y));"

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
	" fInf() = S(fInf()); \
	\ fB(x) = f(S(x));"

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
	int  prog3 $ read "fInf()"

-- "eval" infinite number	
demo09 =
	eval prog3 $ read "fInf()"

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

-- using intTree (infinite tree) to run task
demo14 =
	intTree (buildTree (driveMachine prog1) (read "gEven(fSqr(x))")) [("x", read "S(S(S(Z())))")]

-- successful folding
demo15 =
	putStrLn $ printTree $ foldTree $ buildTree (driveMachine prog1) (read "gEven(fSqr(x))")

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

-- KMP - transformation
demo21 = do
	let (c2, p2) = transform (conf2, prog2)
	putStrLn "\ntransformation:\n"
	putStrLn (show c2)
	putStrLn (show p2)
	
-- KMP - deforestation
demo22 = do
	let (c2, p2) = deforest (conf2, prog2)
	putStrLn "\ndeforest:\n"
	putStrLn (show c2)
	putStrLn (show p2)

-- KMP - supercompilation
demo23 = do
	let (c2, p2) = supercompile (conf2, prog2)
	putStrLn "\nsupercompile:\n"
	putStrLn (show c2)
	putStrLn (show p2)

-- KMP -- transform -- graph
example16a = 
	putStrLn $ printTree $ foldTree $ buildFTree (driveMachine prog2) conf2

-- KMP -- deforest -- graph
example16b = 
	putStrLn $ printTree $ simplify $ foldTree $ buildFTree (driveMachine prog2) conf2

-- KMP -- supercompile -- graph
example16c = 
	putStrLn $ printTree $ simplify $ foldTree $ buildFTree (addPropagation (driveMachine prog2)) conf2

-- all further stuff is for "benchmarking"

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


benchmark0 = map (snd . (run t1)) [1 .. 50]
benchmark1 = map (snd . (run t1t)) [1 .. 50]
benchmark2 = map (snd . (run t1d)) [1 .. 50]
benchmark3 = map (snd . (run t1s)) [1 .. 50]	