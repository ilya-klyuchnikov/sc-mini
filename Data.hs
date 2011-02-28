module Data where

import Data.List

data Expr = Var Name | Ctr Name [Expr] | FCall Name [Expr] | GCall Name [Expr] | Let Name Expr Expr deriving (Eq)
data Contract = Contract Name Pat
data Step a = Transient a | Variants [(Contract, a)] | Stop | Decompose [a] | Fold a Renaming deriving (Show)
data Tree = Node Expr (Step Tree) 
data Pat = Pat Name [Name]
data GFun = GFun Name Pat [Name] Expr
data FFun = FFun Name [Name] Expr
data Program = Program [FFun] [GFun]

-- We use type synonyms to make declarations more clear (and expressive)
type Subst = [(Name, Expr)]
type NameSupply = [Name]
type Name = String
type Renaming = [(Name, Name)]
type Task = (Expr, Program)
type Value = Expr
type Env = [(Name, Value)]

type DriverGen = Program -> Driver
type Driver = NameSupply -> Expr -> Step Expr
	
instance Show Expr where
	show (Var n) = n
	show (Ctr n es) = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
	show (FCall n es) = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
	show (GCall n es) = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
	show (Let v e1 e2) = "let " ++ v ++ " = " ++ (show e1) ++ " in " ++ (show e2)

instance Show FFun where
	show (FFun fn args body) = fn ++ "(" ++ intercalate ", " args ++ ") = " ++ (show body) ++ ";"

instance Show GFun where
	show (GFun gn p args body) = gn ++ "(" ++ intercalate ", " (show p:args) ++ ") = " ++ (show body) ++ ";"

instance Show Pat where
	show (Pat cn vs) = cn ++ "(" ++ intercalate "," vs ++ ")"
	
instance Show Contract where
	show (Contract n p) = n ++ " = " ++ (show p)
	
instance Show Program where
	show (Program fs gs) = intercalate "\n" $ (map show fs) ++ (map show gs)