module Samples1 where

import Data
import Interpreter
	
programText = 	
	" gAdd(Z(), y) = y;\
	\ gAdd(S(x), y) = S(gAdd(x, y));\
	\ gMult(Z(), y) = Z();\
	\ gMult(S(x), y) = gAdd(gMult(x, y), y);\ 
	\ fSqr(x) = gMult(x, x); "

exp1Text = "fSqr(S(S(Z())))"
exp1Text' = "fSqr(S(S(x)))"
exp2Text = "fSqr(S(S(S(Z()))))"
exp3Text = "fSqr(S(S(S(S(Z())))))"

prog :: Prog	
prog = read programText

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