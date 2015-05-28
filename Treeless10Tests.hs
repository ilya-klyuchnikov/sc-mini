module Treeless10Tests where

import TreelessSInt
import qualified Data
import DataIO
import Driving
import qualified Interpreter
import Deforester
import Prototype
import Supercompiler
import Folding

data CName0 = Unit deriving (Show, Eq)
data CName1 = S | Z deriving (Show, Eq)
data CName2 = NoName2 deriving (Show, Eq)

data GName = Pred | Zero deriving (Show, Eq)
data Exp = GVar1 | GCall1 GName Exp | Ctr Ctr deriving (Show, Eq)
data Ctr = Ctr0 CName0 | Ctr1 CName1 Exp | Ctr2 CName2 Exp Exp deriving (Show, Eq)

data Fun = GFun1 GName CName1 Exp deriving (Show)
type Program = [Fun]

gIsGVar1 GVar1   = True
gIsGVar1 (Ctr c) = False

gIsGFun :: Fun -> Bool 
gIsGFun (GFun1 _ _ _) = True

gAnd False _ = False
gAnd True x = x

gCn1Eq :: CName1 -> CName1 -> Bool
gCn1Eq Z cn = gCn1EqZ cn
gCn1Eq S cn = gCn1EqS cn

gCn1EqZ Z = True
gCn1EqZ S = False
gCn1EqS Z = False
gCn1EqS S = True

gGnEq :: GName -> GName -> Bool
gGnEq Zero gn = gGnEqZero gn
gGnEq Pred gn = gGnEqPred gn
gGnEqZero Pred = False
gGnEqZero Zero = True
gGnEqPred Pred = True
gGnEqPred Zero = False

gEval :: Exp -> Program -> Exp        
gEval (Ctr c) p         = Ctr (gEvalCtr c p)
gEval (GCall1 gn ctr) p = gEvalGCall1 ctr p gn 

gEvalCtr :: Ctr -> Program -> Ctr
gEvalCtr (Ctr0 s) p       = Ctr0 s
gEvalCtr (Ctr1 s e) p     = Ctr1 s (gEval e p)
gEvalCtr (Ctr2 s e1 e2) p = Ctr2 s (gEval e1 p) (gEval e2 p)

gEval01 :: Exp -> Program -> Exp -> Exp
gEval01 GVar1 p gv1 = gv1
gEval01 (Ctr c) p gv1 = Ctr (gEvalCtr01 c p gv1)
gEval01 (GCall1 n arg) p gv1 = gEval01GCall1 (gIsGVar1 arg) n arg p gv1

gEval01GCall1 True  n arg p gv1 = gEvalGCall1 gv1 p n
gEval01GCall1 False n arg p gv1 = gEvalGCall1 arg p n

gEvalCtr01 :: Ctr -> Program -> Exp -> Ctr
gEvalCtr01 (Ctr0 s) p fv1 = Ctr0 s
gEvalCtr01 (Ctr1 s e) p fv1 = Ctr1 s (gEval01 e p fv1)
gEvalCtr01 (Ctr2 s e1 e2) p fv1 = Ctr2 s (gEval01 e1 p fv1) (gEval01 e2 p fv1)

gEvalGCall1 :: Exp -> Program -> GName -> Exp
gEvalGCall1 (Ctr ctr) p gn = gEvalGCall1Ctr ctr p gn

gEvalGCall1Ctr :: Ctr -> Program -> GName -> Exp
gEvalGCall1Ctr (Ctr1 cn arg1) p gn = gEvalGCall1a p p gn cn arg1

gEvalGCall1a :: Program -> Program -> GName -> CName1 -> Exp -> Exp
gEvalGCall1a (fun:p1) p gn cn arg1 = gEvalGCall1b (gIsGFun fun) fun p1 p gn cn arg1

gEvalGCall1b :: Bool -> Fun -> Program -> Program -> GName -> CName1 -> Exp -> Exp
gEvalGCall1b True  fun p1 p gn cn arg1 = gEvalGCall1с fun p1 p gn cn arg1
gEvalGCall1b False fun p1 p gn cn arg1 = gEvalGCall1a p1 p gn cn arg1

gEvalGCall1с :: Fun -> Program -> Program -> GName -> CName1 -> Exp -> Exp
gEvalGCall1с (GFun1 gn' cn' e) p1 p gn cn arg1 = gEvalGCall1d (gAnd (gGnEq gn' gn) (gCn1Eq cn' cn)) p1 p gn cn arg1 e
gEvalGCall1d False p1 p gn cn arg1 e = gEvalGCall1a p1 p gn cn arg1
gEvalGCall1d True  p1 p gn cn arg1 e = gEval01 e p arg1

-------- examples ---------

data Unit = U
data Nat = S0 Nat | Z0 Unit

pred (S0 g1) = g1
pred (Z0 g1) = (Z0 g1)
zero (S0 g1) = zero g1
zero (Z0 g1) = (Z0 g1)

ctr0 s  = Ctr (Ctr0 s)
ctr1 s e1 = Ctr (Ctr1 s e1)
ctr2 s e1 e2 = Ctr (Ctr2 s e1 e2)


predProg :: Program
predProg = [
    GFun1 Pred S GVar1,
    GFun1 Pred Z (ctr1 Z GVar1),
    GFun1 Zero S (GCall1 Zero GVar1),
    GFun1 Zero Z (ctr1 Z GVar1)
    ]

-- note - an arg of function may be --ONLY variable-- and this variable may resolve into constructor ONLY
predProgEnc :: Data.Expr
predProgEnc = read 
    " Cons(GFun1(Pred(), S(), GVar1()), \
    \ Cons(GFun1(Pred(), Z(), Ctr(Ctr1(Z(), GVar1()))), \
    \ Cons(GFun1(Zero(), S(), GCall1(Zero(), GVar1())),\
    \ Cons(GFun1(Zero(), Z(), Ctr(Ctr1(Z(), GVar1()))), Nil()))))"

in2 :: Exp
in2 = GCall1 Pred (ctr1 Z (ctr0 Unit))
result2 = gEval in2 predProg
test2 = result2 == Ctr (Ctr1 Z (Ctr (Ctr0 Unit)))
in2Enc :: Data.Expr
in2Enc = read "GCall1(Pred(), Ctr(Ctr1(Z(), Ctr(Ctr0(Unit())))))"
task2Enc = Data.GCall "gEval" [in2Enc, predProgEnc]

in3 :: Exp
in3 = GCall1 Pred (ctr1 S (ctr1 Z (ctr0 Unit)))
result3 = gEval in3 predProg
test3 = result3 == Ctr (Ctr1 Z (Ctr (Ctr0 Unit)))
in3Enc :: Data.Expr
in3Enc = read "GCall1(Pred(), Ctr(Ctr1(S(), Ctr(Ctr1(Z(), Ctr(Ctr0(Unit())))))))"
task3Enc = Data.GCall "gEval" [in3Enc, predProgEnc]

in4 :: Exp
in4 = GCall1 Zero (ctr1 S (ctr1 S (ctr1 Z (ctr0 Unit))))
result4 = gEval in4 predProg
test4 = result4 == Ctr (Ctr1 Z (Ctr (Ctr0 Unit)))
in4Enc :: Data.Expr
in4Enc = read "GCall1(Zero(), Ctr(Ctr1(S(), Ctr(Ctr1(S(), Ctr(Ctr1(Z(), Ctr(Ctr0(Unit())))))))))"
task4Enc = Data.GCall "gEval" [in4Enc, predProgEnc]

tests = [test2, test3, test4]

--- running self-interpreter ---

sintProg :: Data.Program
sintProg = read sint

result2' = Interpreter.eval sintProg task2Enc
result3' = Interpreter.eval sintProg task3Enc
result4' = Interpreter.eval sintProg task4Enc

predCall :: Data.Expr
predCall = read "GCall1(Pred(), Ctr(c))"
--predCall = read "GCall1(Pred(), Ctr(Ctr1(Z(), Ctr(Ctr0(Unit())))))"
predEval = Data.GCall "gEval" [predCall, predProgEnc]
predEval0 = Interpreter.eval sintProg predEval

zeroCall :: Data.Expr
zeroCall = read "GCall1(Zero(), Ctr(c))"
zeroEval = Data.GCall "gEval" [zeroCall, predProgEnc]

predDemo = do
    let (c2, p2) = supercompile (predEval, sintProg)
    putStrLn "\nsupercompile:\n"
    putStrLn (">>" ++ show c2)
    putStrLn (">>" ++ show p2)

zeroDemo = do
    let (c2, p2) = supercompile (zeroEval, sintProg)
    putStrLn "\nsupercompile:\n"
    putStrLn (">>" ++ show c2)
    putStrLn (">>" ++ show p2)


demo12 =
    putStrLn $ printTree $ buildTree (addPropagation $ driveMachine sintProg) predEval

demo13 =
    putStrLn $ printTree $ simplify $ foldTree $ buildTree (addPropagation $ driveMachine sintProg) predEval

demo15 =
    putStrLn $ printTree $ simplify $ foldTree $ buildTree (addPropagation $ driveMachine sintProg) zeroEval
