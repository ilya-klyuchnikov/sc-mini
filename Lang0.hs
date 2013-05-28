module Lang0 where

-- The idea of such representation of SLL is taken from "Removing inherited limits" by Mogensen.
-- Universe of values
data Value = Con Int [Value]
data Exp = Var Int | Fap Int [Exp] | Cap Int [Exp] | Case Exp [Exp]
type Prog = [Exp]
type Env = [Value]

apply f p args = eval (p !! f) args p

eval :: Exp -> Env -> Prog -> Value
eval (Var n) vs p = 
	vs !! n
eval (Fap n es) vs p = 
	apply n p (map (\x -> eval x vs p) es)
eval (Cap n es) vs p =
	Con n (map (\x -> eval x vs p) es)
eval (Case e es) vs p | Con n vs1 <- eval e vs p = 
	eval (es !! n) (vs1 ++ vs) p

-- simple program
-- app x y = case x of {Nil -> y; Cons x1 xs -> Cons(x1, app(xs, y);}
-- app 0 1 = case 0 of {Nil -> 1; Cons 0 1 -> Cons(0, app(1, 3))}
-- fn = [app], cs = [Nil, Cons, A]

-- this is how the whole program is represented
prog :: Prog
prog = [Case (Var 0) [Var 1, Cap 1 [Var 0, Fap 0 [Var 1, Var 3]]]]


