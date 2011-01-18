module Interpreter where

import Data
import Data.List
import Data.Maybe

isGround :: Expr -> Bool
isGround (Ctr _ args) = and $ map isGround args 
isGround _ = False

intStep :: Program -> Expr -> Expr
intStep p (Ctr name args) = 
	Ctr name (grounds ++ (int p x : xs)) where 
		(grounds, x : xs) = span isGround args

intStep p (FCall name args) = 
	(subst (zip vs args) t) where 
		(FFun _ vs t) = fFun p name

intStep p (GCall gname (Ctr cname cargs : args)) = 
	subst (zip (cvs ++ vs) (cargs ++ args)) t where 
		(GFun _ (Pat _ cvs) vs t) = gFun p gname cname

intStep p (GCall gname (e:es)) = 
	(GCall gname (intStep p e : es))
	
intStep p (Let x e1 e2) =
	subst [(x, e1)] e2

int :: Program -> Expr -> Expr
int p e | isGround e = e
		| otherwise = int p (intStep p e) 