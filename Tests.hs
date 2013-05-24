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
import Deforester

appProg :: Program
appProg = read
	" gApp(Cons(x, xs), ys) = Cons(x, gApp(xs, ys));\
	\ gApp(Nil(), ys) = ys; \
	\ gApp1(Cons(x, xs), ys) = Cons(x, gApp2(xs, ys));\
	\ gApp1(Nil(), ys) = ys;\
    \ gApp2(Cons(x, xs), ys) = Cons(x, gApp1(xs, ys));\
	\ gApp2(Nil(), ys) = ys;"


demo01 =
	putStrLn $ printTree $ foldTree $ buildTree (driveMachine appProg) (read "gApp(x, y)")

demo02 =
	putStrLn $ printTree $ foldTree $ buildTree (driveMachine appProg) (read "gApp(gApp(x, y), z)")

demo03 =
	putStrLn $ printTree $ foldTree $ buildTree (driveMachine appProg) (read "gApp1(x, y)")

demo04 =
	putStrLn $ printTree $ foldTree $ buildTree (driveMachine appProg) (read "gApp1(gApp1(x, y), z)")

{-
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
-}