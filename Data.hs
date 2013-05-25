module Data where

type Name = String
data Expr = Var Variable | Ctr Name [Expr] | FCall Name [Expr] | GCall Name [Expr] deriving (Eq)
-- Variable is either named variable or selector variable.
data Variable = NVar Name | SVar Name Variable deriving (Eq, Ord)
data Pat = Pat Name [Variable] deriving (Eq)
data GDef = GDef Name Pat [Variable] Expr deriving (Eq)
data FDef = FDef Name [Variable] Expr deriving (Eq)
data Program = Program [FDef] [GDef] deriving (Eq)

type Renaming = [(Variable, Variable)]
type Subst = [(Variable, Expr)]

type Task = (Expr, Program)

data Contraction = Contraction Variable Pat
data Step a = Transient a | Variants [(Contraction, a)] | Stop | Decompose Name [a] | Fold a Renaming
data Graph a = Node a (Step (Graph a))
type Tree a = Graph a
type Node a = Graph a

type Machine a = a -> Step a
