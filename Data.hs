module Data where

type Name = String
data Expr = Var Name | Ctr Name [Expr] | FCall Name [Expr] | GCall Name [Expr] | 
    Let (Name, Expr) Expr deriving (Eq)
data Pat = Pat Name [Name] deriving (Eq)
data GDef = GDef Name Pat [Name] Expr deriving (Eq)
data FDef = FDef Name [Name] Expr deriving (Eq)
data Program = Program [FDef] [GDef] deriving (Eq)

type Renaming = [(Name, Name)]
type Subst = [(Name, Expr)]
type NameSupply = [Name]

type Conf = Expr
type Value = Expr
type Task = (Conf, Program)
type Env = [(Name, Value)]

data Contraction = Contraction Name Pat
data TestResult = Match Pat | EqTest Bool
data Step a = Transient (Maybe TestResult) a | Variants [(Contraction, a)]
			| Stop a | Decompose ([a] -> a) [a]
data Edge a = ETransient (Maybe TestResult) (Graph a) | EVariants [(Contraction, Graph a)] 
			| EDecompose ([a] -> a) [Graph a] | EFold (Graph a) Renaming
data Graph a = Node a (Edge a) | Leaf a
type Tree a = Graph a
type Node a = Tree a

type Machine a = NameSupply -> a -> Step a
