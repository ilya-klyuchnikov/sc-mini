{-# LANGUAGE DeriveDataTypeable #-}

module Data where

import Data.Data

type Name = String
data Expr = Var Name | Ctr Name [Expr] | FCall Name [Expr] | GCall Name [Expr] | Let (Name, Expr) Expr deriving (Eq, Data)
data Pat = Pat Name [Name] deriving (Eq, Data)
data GDef = GDef Name Pat [Name] Expr deriving (Eq, Data)
data FDef = FDef Name [Name] Expr deriving (Eq, Data)
data Program = Program [FDef] [GDef] deriving (Eq, Data)

type Renaming = [(Name, Name)]
type Subst = [(Name, Expr)]
type NameSupply = [Name]

type Conf = Expr
type Value = Expr
type Task = (Conf, Program)
type Env = [(Name, Value)]

data Contract = Contract Name Pat
data Step a = Transient a | Variants [(Contract, a)] | Stop | Decompose [a] | Fold a Renaming
data Graph a = Node a (Step (Graph a))
type Tree a = Graph a
type Node a = Tree a

type Machine a = NameSupply -> a -> Step a
