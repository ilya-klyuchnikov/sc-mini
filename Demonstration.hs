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
import ATransformer
import Deforester

p1Text = " gAdd(Z(), y) = y;\
	\ gAdd(S(x), y) = S(gAdd(x, y));\
	\ gMult(Z(), y) = Z();\
	\ gMult(S(x), y) = gAdd(y, gMult(x, y));\ 
	\ fSqr(x) = gMult(x, x); \
	\ gEven(Z()) = True();\
	\ gEven(S(x)) = gOdd(x);\
	\ gOdd(Z()) = False();\
	\ gOdd(S(x)) = gEven(x);"

p1 :: Program
p1 = read p1Text

p2Text = " gEqSymb(A(), y) = gEqA(y);\
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
	
p2 :: Program
p2 = read p2Text

conf0Text = "gEven(fSqr(S(x)))"

conf0 :: Expr
conf0 = read conf0Text


conf1Text = "gEven(fSqr(x))"

conf1 :: Expr
conf1 = read conf1Text

conf2Text = "fMatch(Cons(A(), Cons(A(), Cons(B(), Nil()))), str)"

conf2 :: Expr
conf2 = read conf2Text

one :: Expr
one = read "S(Z())"

two :: Expr
two = read "S(S(Z()))"

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
	putStrLn $ show (intC p1 goal1)

example2 :: IO ()
example2 = do
	putStr (show goal1)
	putStr " => "
	putStrLn $ show (intC p1 goal2)
	
example3 :: IO ()
example3 = do
	putStr (show goal1)
	putStr " => "
	putStrLn $ show (intC p1 goal3)
	
example4 :: IO ()
example4 = do
	traceInt p1 goal1
	
example5 :: IO ()
example5 = do
	traceInt p1 conf0

state1 = (conf1, p1)
state1t = transform state1
state1d = deforest state1
state1s = supercompile state1

state2 = (conf2, p2)
example10 = transform state2
example11 = deforest state2
example12 = supercompile state2

number 0 = Ctr "Z" []
number n = Ctr "S" [number (n - 1)]

run st n = intFacade st [("x", number n)]

main = do
	example1
	putStrLn ""
	example2
	putStrLn ""
	example3
	putStrLn ""
	example4
	putStrLn ""
	example5