module Samples1 where

import Data
import Interpreter
	
programText = 	
	" gAdd(Z(), y) = y;\
	\ gAdd(S(x), y) = S(gAdd(x, y));\
	\ gMult(Z(), y) = Z();\
	\ gMult(S(x), y) = gAdd(y, gMult(x, y));\ 
	\ fSqr(x) = gMult(x, x); \	
	\ gEven(Z()) = True();\
	\ gEven(S(x)) = gOdd(x);\
	\ gOdd(Z()) = False();\
	\ gOdd(S(x)) = gEven(x);\
	\ gEq(Z(), y) = gEqZ(y);\
	\ gEq(S(x), y) = gEqS(y, x);\
	\ gEqZ(Z()) = True();\
	\ gEqZ(S(x)) = False();\
	\ gEqS(Z(), x) = False();\
	\ gEqS(S(y), x) = gEq(x, y);"

exp0Text = "gAdd(S(S(Z())), S(S(Z())))"
exp1Text = "gEven(fSqr(S(Z())))"
exp1Text' = "gEven(fSqr(S(x)))"
exp2Text = "fSqr(S(S(S(Z()))))"
exp3Text = "fSqr(S(S(S(S(Z())))))"

prog :: Program	
prog = read programText

exp0 :: Expr
exp0 = read exp0Text

exp1 :: Expr
exp1 = read exp1Text

exp1' :: Expr
exp1' = read exp1Text'

exp2 :: Expr
exp2 = read exp2Text

exp3 :: Expr
exp3 = read exp3Text

args1 :: [Expr]
args1 = [read "S(S(Z()))"]

args2 :: [Expr]
args2 = [read "S(S(S(Z())))"]

args3 :: [Expr]
args3 = [read "S(S(S(S(Z()))))"]


{-
intMainC prog "fSqr" args1
-}