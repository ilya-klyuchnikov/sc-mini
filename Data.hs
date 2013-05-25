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
--type Env = [(Name, Expr)]

data Contraction = Contraction Variable Pat
data TestResult = Match Pat | EqTest Bool
data Step a = Transient a | Variants [(Contraction, a)]
			| Stop a | Decompose ([a] -> a) [a]
data Edge a = ETransient (Graph a) | EVariants [(Contraction, Graph a)] 
			| EDecompose ([a] -> a) [Graph a] | EFold (Graph a) Renaming
data Graph a = Node a (Edge a) | Leaf a
type Tree a = Graph a
type Node a = Tree a

type Machine a = a -> Step a
