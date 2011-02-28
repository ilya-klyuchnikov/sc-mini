module Data where

import Data.List

data Expr = Var Name | Ctr Name [Expr] | FCall Name [Expr] | GCall Name [Expr] | Let (Name, Expr) Expr deriving (Eq)
data Pat = Pat Name [Name]
data GDef = GDef Name Pat [Name] Expr
data FDef = FDef Name [Name] Expr
data Program = Program [FDef] [GDef]

data Contract = Contract Name Pat
data Step a = Transient a | Variants [(Contract, a)] | Stop | Decompose [a] | Fold a Renaming deriving (Show)
data Graph a = Node a (Step (Graph a))
type Tree a = Graph a
type Node a = Tree a

-- We use type synonyms to make declarations more clear (and expressive)
type Name = String
type Renaming = [(Name, Name)]
type Subst = [(Name, Expr)]
type NameSupply = [Name]

type Conf = Expr
type Value = Expr
type Task = (Conf, Program)
type Env = [(Name, Value)]

type Machine a = NameSupply -> a -> Step a
type MachineGen p a = p -> Machine a
	
instance Show Expr where
	show (Var n) = n
	show (Ctr n es) = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
	show (FCall n es) = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
	show (GCall n es) = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
	show (Let (v, e1) e2) = "let " ++ v ++ " = " ++ (show e1) ++ " in " ++ (show e2)

instance Show FDef where
	show (FDef fn args body) = fn ++ "(" ++ intercalate ", " args ++ ") = " ++ (show body) ++ ";"

instance Show GDef where
	show (GDef gn p args body) = gn ++ "(" ++ intercalate ", " (show p:args) ++ ") = " ++ (show body) ++ ";"

instance Show Pat where
	show (Pat cn vs) = cn ++ "(" ++ intercalate "," vs ++ ")"
	
instance Show Contract where
	show (Contract n p) = n ++ " = " ++ (show p)
	
instance Show Program where
	show (Program fs gs) = intercalate "\n" $ (map show fs) ++ (map show gs)