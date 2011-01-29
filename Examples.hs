module Examples where
	
import Data
import Driving
import Interpreter
import TreeInterpreter
import Supercompiler
import Folding
import Data.List
import Data.Maybe

driveTest p ns t = drive p ns t

{-
program1 = Program [] [
	GFun "gApp" (Pat "Nil" []) ["ys"] (Var "ys"),
	GFun "gApp" (Pat "Cons" ["x", "xs"]) ["ys"] (Ctr "Cons" [(Var "x") , GCall "gApp" [(Var "xs"), (Var "ys")]])
	]
	
program2 = Program [] [
	GFun "app" (Pat "Nil" []) ["ys"] (Var "ys"),
	GFun "app" (Pat "Cons" ["x", "xs"]) ["ys"] (Ctr "Cons" [(Var "x") , GCall "app" [(Var "xs"), (Var "ys")]]),
	GFun "rev" (Pat "Nil" []) [] (Ctr "Nil" []),
	GFun "rev" (Pat "Cons" ["x", "xs"]) [] (GCall "app" [GCall "rev" [Var "xs"], Ctr "Cons" [Var "x", (Ctr "Nil" [])] ])
	]

goalRev = GCall "rev" [Var "es"]

-- Sorensen. Example 3.6.2
-- g (Cons x xs) y = h y
-- g Nil y = h y
-- h (Cons z zs) = zs
-- h Nil = Nil
program10 = Program [] [
	GFun "g" (Pat "Cons" ["x", "xs"]) ["y"] (GCall "h" [Var "y"]),
	GFun "g" (Pat "Nil" []) ["y"] (GCall "h" [Var "y"]),
	GFun "h" (Pat "Cons" ["z", "zs"]) [] (Var "zs"),
	GFun "h" (Pat "Nil" []) ["y"] (Ctr "Nil" [])
	]

goal1 = GCall "gApp" [Ctr "Nil" [], Ctr "Cons" [Ctr "A" [], Ctr "Nil" []]]
goal2 = GCall "gApp" [Ctr "Cons" [Ctr "A" [], Ctr "Nil" []], Ctr "Nil" []]

goal0 = GCall "gApp" [Var "v", Var "xs"]
goal01 = GCall "gApp" [GCall "gApp" [Var "x", Var "y"], Var "z"]


goal3 = GCall "or" [Ctr "False" [], Ctr "True" []]
goal4 = Ctr "False" []
goal5 = GCall "or" [Var "x", Var "x"]

test1 = int program1 goal1
test2 = int program1 goal2

goal10 = GCall "g" [Var "v", Var "v"]
test10 = (buildTree program10 nameSupply goal10)

run program goal = putStr $ unlines $ take 100 $ pprintTree "" "" (buildTree program nameSupply goal)
runFold program goal = putStr $ unlines $ take 100 $ pprintTree "" "" (foldTree (buildTree program nameSupply goal))
runFold1 program goal = putStr $ unlines $ take 100 $ pprintTree "" "" (foldTree (buildFTree program nameSupply goal))

tree :: Tree
tree = (foldTree (buildFTree program1 nameSupply goal01))

p = (p', t') where
	(p', t', _) = res nameSupply [] (simplify tree)
	
printTree t = putStr $ unlines $ take 100 $ pprintTree "" "" t

-- all variables shoul be different!
main = do 
	--run program10 goal10
	putStr "\n"
	--run program1 goal0
	putStr "\n"
	--run program1 goal01
	putStr "\n"
	--runFold program1 goal01
	----(foldTree (buildTree program1 nameSupply goal1))
	putStr (show (intTree [("x", Ctr "Cons" [Ctr "A" [], Ctr "Nil" []]), ("y", Ctr "Nil" []), ("z", Ctr "Nil" [])] (buildTree program1 nameSupply goal01)))
	putStr "\n"
	--runFold program2 goalRev
	putStr "\n"
	runFold1 program2 goalRev
-}