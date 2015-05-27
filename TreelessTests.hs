module TreelessTests where

import Treeless
import Test.HUnit

-- IN PROGRESS - not working for now

data List a = Nil | Cons a (List a)

app Nil ys0 = ys0
app (Cons gv0 gv1) fv0 = Cons gv0 (app gv1 fv0)

id1 v1 = id2 v1
id2 v1 = id3 v1
id3 v1 = v1

{-
appProg :: Program
appProg = [
    GFun 1 "app" (Ptr0 "Nil") (FVar 0),
    GFun 3 "app" (Ptr2 "Cons") (Ctr (Ctr2 "Cons" [(GVar 0), GCall "app" [GVar 1, FVar 0]]))
    ]
-}

idProg :: Program
idProg = [
    FFun1 "id1" (FCall1 "id2" (FVar 0)),
    FFun1 "id2" (FVar 0)
    ]

ctr :: String -> [Exp] -> Exp
ctr s [] = Ctr (Ctr0 s)
ctr s [e1] = Ctr (Ctr1 s [e1])
ctr s [e1, e2] = Ctr (Ctr2 s [e1, e2])

{-
in1 = GCall "app" [ctr "Nil" [], ctr "Nil" []]
result1 = eval appProg in1 [] []

in2 = GCall "app" [ctr "Nil" [], ctr "Cons" [ctr "A" [], ctr "Nil" []]]
result2 = eval appProg in2 [] []

in3 = GCall "app" [ctr "Cons" [ctr "A" [], ctr "Nil" []], ctr "Nil" []]
result3 = eval appProg in3 [] []
-}

in10 :: Exp
in10 = FCall1 "id1" (ctr "Nil" [])
result10 = eval idProg in10 [] []