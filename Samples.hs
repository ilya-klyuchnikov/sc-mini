module Samples where

import Data
import Driving
import Interpreter
import TreeInterpreter
import Supercompiler
import Folding
import Data.List
import Data.Maybe
import Generator
import Examples

conf1 = Config True True 100

load :: (String, String) -> State
load (p1, p2) = (read p1, read p2)

runTask (g, p) = 
	"was:\n" ++ (show goal1) ++ "\n" ++ (show program1) ++ "\n\ntransformed:\n" ++ (show goal2) ++ "\n" ++ (show program2) ++ "\n"
	where
		(goal1, program1) = (read g, read p)
		(goal2, program2) = supercompile conf1 (goal1, program1)
		
bt (g, p) =
	unlines $ take 100 $ pprintTree "" "" $ foldTree $ buildFTree p nameSupply g

t1 = ("gApp(gApp(x, y), z)",
	" gApp(Nil(), ys) = ys;\
	\ gApp(Cons(x, xs), ys) = Cons(x, gApp(xs, ys));")
	
t2 = ( "fMatch(Cons(A(), Nil()), str)", --"fMatch(Cons(A(), Cons(A(), Cons(B(), Nil()))), str)",
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
	\ gN(Cons(s, ss), op) = gM(op, ss, op, ss);")
	
main = do
	putStr (runTask t2)