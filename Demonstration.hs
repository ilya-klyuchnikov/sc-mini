module Demonstration where

import Data
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

prog1Text = " gAdd(Z(), y) = y;\
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

prog1 :: Program
prog1 = read prog1Text

prog2Text = " gEqSymb(A(), y) = gEqA(y);\
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
	
prog2 :: Program
prog2 = read prog2Text

conf0Text = "gEven(fSqr(S(x)))"

conf0 :: Expr
conf0 = read conf0Text


conf1Text = "gEven(fSqr(x))"

conf1 :: Expr
conf1 = read conf1Text

conf2Text = "fMatch(Cons(A(), Cons(A(), Cons(B(), Nil()))), s)"

conf3T = "fMatch(Cons(A(), Cons(A(), Nil())), s)"

conf2 :: Expr
conf2 = read conf2Text

goal1 :: Expr
goal1 = read "fSqr(S(S(Z())))"

goal2 :: Expr
goal2 = read "fSqr(S(S(S(Z()))))"

goal3 :: Expr
goal3 = read "fSqr(S(S(S(S(Z())))))"

example1 :: IO ()
example1 = do
	putStr (show goal1)
	putStr " => "
	putStrLn $ show (intC prog1 goal1)

example2 :: IO ()
example2 = do
	putStr (show goal1)
	putStr " => "
	putStrLn $ show (intC prog1 goal2)
	
example3 :: IO ()
example3 = do
	putStr (show goal1)
	putStr " => "
	putStrLn $ show (intC prog1 goal3)
	
example4 :: IO ()
example4 = do
	traceInt prog1 goal1
	
example5 :: IO ()
example5 = do
	traceInt prog1 conf0
	
example6 = 
	putStrLn $ show $ drive prog1 nameSupply (read "gOdd(gAdd(x, gMult(x, S(x))))")
example7 = 
	putStrLn $ show $ drive prog1 nameSupply (read "gOdd(S(gAdd(x1, gMult(x, S(x)))))")

example8 :: IO ()
example8 = 
	putStrLn $ printTree $ buildTree prog1 nameSupply conf1

example9 :: IO ()
example9 = do
	putStrLn $ show $ intTree [("x", read "S(S(Z()))")] (buildTree prog1 nameSupply conf1)
	putStrLn $ show $ intTree [("x", read "S(S(S(Z())))")] (buildTree prog1 nameSupply conf1)
	
example10 :: IO ()
example10 = 
	putStrLn $ printTree $ foldTree $ buildTree prog1 nameSupply conf1
	
example11 :: IO ()
example11 =
	putStrLn $ printTree $ foldTree $ buildTree prog1 nameSupply (read "gAdd1(x, y)")
	
example12 :: IO ()
example12 =
	putStrLn $ printTree $ foldTree $ buildFTree drive prog1 nameSupply (read "gAdd1(x, y)")
	
example12a :: IO ()
example12a =
	putStrLn $ printTree $ foldTree $ buildFTree drive prog1 nameSupply conf1
	
example13 :: IO ()
example13 = do
	putStrLn "just transformation"
	putStrLn "before:\n"
	putStrLn (show conf1)
	putStrLn (show prog1)
	let (c2, p2) = transform (conf1, prog1)
	putStrLn "\nafter:\n"
	putStrLn (show c2)
	putStrLn (show p2)

example14 :: IO()
example14 = do
	putStrLn "just transformation"
	putStrLn "before:\n"
	putStrLn (show conf2)
	putStrLn (show prog2)
	let (c2, p2) = transform (conf2, prog2)
	putStrLn "\nafter:\n"
	putStrLn (show c2)
	putStrLn (show p2)
	
example15 :: IO()
example15 = do
	putStrLn "deforestantion"
	putStrLn "before:\n"
	putStrLn (show conf2)
	putStrLn (show prog2)
	let (c2, p2) = deforest (conf2, prog2)
	putStrLn "\nafter:\n"
	putStrLn (show c2)
	putStrLn (show p2)
	
example16 :: IO()
example16 = do
	putStrLn "supercompilation"
	putStrLn "before:\n"
	putStrLn (show conf2)
	putStrLn (show prog2)
	let (c2, p2) = supercompile (conf2, prog2)
	putStrLn "\nafter:\n"
	putStrLn (show c2)
	putStrLn (show p2)
	
example16a :: IO()
example16a = 
	putStrLn $ printTree $ simplify $ foldTree $ buildFTree (propagate drive) prog2 nameSupply conf2
	
example16b :: IO()
example16b = 
	putStrLn $ printTree $ simplify $ foldTree $ buildFTree drive prog2 nameSupply ( read conf3T)
	
--example17 :: IO()
example17 =
	drive prog2 nameSupply (read "gX(s, A(), t, t, s)")

state1 = (conf1, prog1)
state1t = transform state1
state1d = deforest state1
state1s = supercompile state1

def (e, p) = simplify $ foldTree $ buildFTree drive p nameSupply e
tr (e, p) = foldTree $ buildFTree drive p nameSupply e

t1d = def state1
t1t = tr state1

state2 = (conf2, prog2)
example101 = transform state2
example102 = deforest state2
example103 = supercompile state2

number 0 = Ctr "Z" []
number n = Ctr "S" [number (n - 1)]

run st n = intFacade st [("x", number n)]

e1 :: Expr 
e1 = read "gEven(gAdd(x, gMult(v1, x)))"

e2 :: Expr 
e2 = read "gEven(gAdd(v3, gMult(v1, x)))"


main = do
	example1
	putStrLn ""
	example2
	putStrLn ""
	example3
	putStrLn ""
	example4
	--putStrLn ""
	--example5
	putStrLn ""
	example6
	putStrLn ""
	example7
	putStrLn ""
	example8
	putStrLn ""
	example9
	putStrLn ""
	example10
	putStrLn ""
	example11
	putStrLn ""
	example12
	putStrLn ""
	example13
	putStrLn ""