module Tests where

import Data
import DataUtil
import DataIO
import Driving
import Interpreter
import TreeInterpreter
import Folding
import Data.List
import Data.Maybe
import Generator
import Deforester

appProg :: Program
appProg = read
	" gApp(CONS(x, xs), ys) = CONS(x, gApp(xs, ys));\
	\ gApp(NIL(), ys) = ys;"

demo1 = do
	let (c2, p2) = deforest ((read "gApp(x, y)"), appProg)
	putStrLn "\ndeforestation:\n"
	putStrLn (show c2)
	putStrLn (show p2)

demo2 = do
	let (c2, p2) = deforest ((read "gApp(gApp(x, y), z)"), appProg)
	putStrLn "\ndeforestation:\n"
	putStrLn (show c2)
	putStrLn (show p2)