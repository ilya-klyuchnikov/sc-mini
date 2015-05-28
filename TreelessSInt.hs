module TreelessSInt where

sint = 
    " gIsGVar1(GVar1())   = True();\
    \ gIsGVar1(Ctr(c)) = False();\

    \ gIsGFun(GFun1(x, y, z)) = True();\

    \ gAnd(False(), x) = False();\
    \ gAnd(True(), x) = x;\

    \ gCn1Eq(Z(), cn) = gCn1EqZ(cn);\
    \ gCn1Eq(S(), cn) = gCn1EqS(cn);\

    \ gCn1EqZ(Z()) = True();\
    \ gCn1EqZ(S()) = False();\
    \ gCn1EqS(Z()) = False();\
    \ gCn1EqS(S()) = True();\

    \ gGnEq(Zero(), gn) = gGnEqZero(gn);\
    \ gGnEq(Pred(), gn) = gGnEqPred(gn);\
    \ gGnEqZero(Pred()) = False();\
    \ gGnEqZero(Zero()) = True();\
    \ gGnEqPred(Pred()) = True();\
    \ gGnEqPred(Zero()) = False();\

    \ gEval(Ctr(c), p)          = Ctr(gEvalCtr(c, p));\
    \ gEval(GCall1(gn, ctr), p) = gEvalGCall1(ctr, p, gn);\ 

    \ gEvalCtr(Ctr0(s), p)         = Ctr0(s);\
    \ gEvalCtr(Ctr1(s, e), p)      = Ctr1(s, gEval(e, p));\
    \ gEvalCtr(Ctr2(s, e1, e2), p) = Ctr2(s, gEval(e1, p), gEval(e2, p));\

    \ gEval01(GVar1(), p, gv1) = gv1;\
    \ gEval01(Ctr(c), p, gv1) = Ctr(gEvalCtr01(c, p, gv1));\
    \ gEval01(GCall1(n, arg), p, gv1) = gEval01GCall1(gIsGVar1(arg), n, arg, p, gv1);\

    \ gEval01GCall1(True(),  n, arg, p, gv1) = gEvalGCall1(gv1, p, n);\
    \ gEval01GCall1(False(), n, arg, p, gv1) = gEvalGCall1(arg, p, n);\

    \ gEvalCtr01(Ctr0(s), p, fv1) = Ctr0(s);\
    \ gEvalCtr01(Ctr1(s, e), p, fv1) = Ctr1(s, gEval01(e, p, fv1));\
    \ gEvalCtr01(Ctr2(s, e1, e2), p, fv1) = Ctr2(s, gEval01(e1, p, fv1), gEval01(e2, p, fv1));\

    \ gEvalGCall1(Ctr(ctr), p, gn) = gEvalGCall1Ctr(ctr, p, gn);\

    \ gEvalGCall1Ctr(Ctr1(cn, arg1), p, gn) = gEvalGCall1a(p, p, gn, cn, arg1);\

    \ gEvalGCall1a(Cons(fun,p1), p, gn, cn, arg1) = gEvalGCall1b(gIsGFun(fun), fun, p1, p, gn, cn, arg1);\

    \ gEvalGCall1b(True(),  fun, p1, p, gn, cn, arg1) = gEvalGCall1с(fun, p1, p, gn, cn, arg1);\
    \ gEvalGCall1b(False(), fun, p1, p, gn, cn, arg1) = gEvalGCall1a(p1, p, gn, cn, arg1);\

    \ gEvalGCall1с(GFun1(gn1, cn1, e), p1, p, gn, cn, arg1) = gEvalGCall1d(gAnd(gGnEq(gn1, gn), gCn1Eq(cn1, cn)), p1, p, gn, cn, arg1, e);\

    \ gEvalGCall1d(False(), p1, p, gn, cn, arg1, e) = gEvalGCall1a(p1, p, gn, cn, arg1);\
    \ gEvalGCall1d(True(),  p1, p, gn, cn, arg1, e) = gEval01(e, p, arg1);"
