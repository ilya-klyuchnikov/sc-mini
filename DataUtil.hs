module DataUtil where
	
import Data
import Maybe
import Char
import List
-- TODO: get rid of this imports
import Text.ParserCombinators.ReadP

readVar1 :: ReadS Name 
readVar1 i = concat [lex s1 | (",", s1) <- lex i] 

nameSupply = ["v" ++ (show i) | i <- [1 ..] ]
expr (Node e _) = e
step (Node _ s) = s
unused (Contract _ (Pat _ vs)) = (\\ vs)

fDef :: Program -> Name -> FDef
fDef (Program fs _) fname = head [f | f@(FDef x _ _) <- fs, x == fname]

gDefs :: Program -> Name -> [GDef]
gDefs (Program _ gs) gname = [g | g@(GDef x _ _ _) <- gs, x == gname]

gDef :: Program -> Name -> Name -> GDef
gDef p gname cname = head [g | g@(GDef _ (Pat c _) _ _) <- gDefs p gname, c == cname]

subst :: Subst -> Expr -> Expr
subst sub (Var x)  = maybe (Var x) id (lookup x sub)
subst sub (Ctr name args)  = Ctr name (map (subst sub) args)
subst sub (FCall name args)  = FCall name (map (subst sub) args)
subst sub (GCall name args) = GCall name (map (subst sub) args)
subst sub (Let (x, e1) e2) = Let (x, (subst sub e1)) (subst sub e2)

isCall :: Expr -> Bool
isCall (FCall _ _) = True
isCall (GCall _ _) = True
isCall _ = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _ = False

isValue :: Expr -> Bool
isValue (Ctr _ args) = and $ map isValue args 
isValue _ = False

-- list of local fails and local successes 
rawRenaming :: (Expr, Expr) -> [Maybe (Name, Name)]
rawRenaming ((Var x), (Var y)) = [Just (x, y)]
rawRenaming ((Ctr n1 args1), (Ctr n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming ((FCall n1 args1), (FCall n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming ((GCall n1 args1), (GCall n2 args2)) | n1 == n2 = concat $ map rawRenaming $ zip args1 args2
rawRenaming (Let (v, e1) e2, Let (v', e1') e2') = rawRenaming (e1, e1') ++ rawRenaming (e2, subst [(v, Var v')] e2')
rawRenaming _  = [Nothing]

-- global success = no local failures ++ all local successes are the same
renaming :: Expr -> Expr -> Maybe[(Name, Name)]
renaming e1 e2 = f $ partition isNothing $ rawRenaming (e1, e2) where
	f (x:_, _) = Nothing
	f (_, ps) = g gs1 gs2
		where 
			gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy (\(a, b) (c, d) -> compare a c) $ nub $ catMaybes ps
			gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy (\(a, b) (c, d) -> compare b d) $ nub $ catMaybes ps
	g xs ys = if all ((== 1) . length) xs && all ((== 1) . length) ys then Just (concat xs) else Nothing
	
isRenaming e1 e2 = isJust $ renaming e1 e2

size :: Expr -> Integer
size (Var _) = 1
size (Ctr _ args) = 1 + sum (map size args)
size (FCall _ args) = 1 + sum (map size args)
size (GCall _ args) = 1 + sum (map size args)
size (Let (_, e1) e2) = 1 + (size e1) + (size e2)

-- a SET (so, without duplicates) of all variable names encountered in a given expression
-- variables are in the order they are encountered
vnames :: Expr -> [Name]
vnames = nub . vnames1

-- a LIST (so, with possible duplicates) of all variable names encounterd in a given expressions
-- variables are in the order they are encountered
vnames1 :: Expr -> [Name]
vnames1 (Var v) = [v]
vnames1 (Ctr _ args)   = concat $ map vnames1 args
vnames1 (FCall _ args) = concat $ map vnames1 args
vnames1 (GCall _ args) = concat $ map vnames1 args
vnames1 (Let (_, e1) e2) = vnames1 e1 ++ vnames1 e2

isRepeated vn e = (length $ filter (== vn) (vnames1 e)) > 1

-- READ/SHOW
	
instance Read Expr where
	readsPrec _ s = readsExpr s

instance Read Program where
	readsPrec _ s = readProgram s

readExpr :: ReadP Expr
readExpr = readS_to_P readsExpr

readsExpr :: ReadS Expr
readsExpr i = catMaybes [merge n (readArgs s)  s | (n, s) <- lex i] where
	merge n@('g':_) [(args, s1)] _ = Just (GCall n args, s1)
	merge n@('f':_) [(args, s1)] _ = Just (FCall n args, s1)
	merge n@(x:_) [(args, s1)] _ | isUpper x = Just (Ctr n args, s1)
	merge n@(x:_) [] s | isLower x = Just (Var n, s)
	merge _ _ _ = Nothing

readArgs :: ReadS [Expr]
readArgs = readP_to_S $ between (char '(') (char ')') (sepBy readExpr (char ','))

readVars :: ReadS [Name]
readVars = readP_to_S $ between (char '(') (char ')') (sepBy (readS_to_P lex) (char ','))

readFDef :: ReadS FDef
readFDef i = [ (FDef n vars body, s4) | 
	(n@('f':_), s) <- lex i, 
	(vars, s1) <- readVars s, 
	("=", s2) <- lex s1,
	(body, s3) <- readsExpr s2,
	(";", s4) <- lex s3]
	
readSPat :: ReadS Pat
readSPat i = [(Pat n vars, s2)|
	(n, s) <- lex i,
	(vars, s2) <- readVars s]
-- read g-function
readGDef i = [ (GDef n p vs body, s6) |
	(n@('g':_), s) <- lex i,
	("(", s1) <- lex s,
	(p, s2) <- readSPat s1,
	(vs, s3) <- readP_to_S (manyTill (readS_to_P readVar1)  (char ')')) s2,
	("=", s4) <- lex s3,
	(body, s5) <- readsExpr s4,
	(";", s6) <- lex s5
	]

readProgram s = [readP1 (Program [] []) s]

readP1 p@(Program fs gs) s = next (readFDef s) (readGDef s) where
	next [(f, s1)] _ = readP1 (Program (fs++[f]) gs) s1
	next _ [(g, s1)] = readP1 (Program fs (gs++[g])) s1
	next _ _ = (p, s)
	
printTree t = unlines $ take 1000 $ pprintTree "" "" t

pprintTree :: String -> String -> Graph Conf -> [String]
pprintTree indent msg (Node expr next) = make next where
	make (Fold _ ren) = (indent ++ msg) : [indent ++ "|__" ++  (show expr) ++ "__↑" ++ (show ren)]
	make Stop = (indent ++ msg) : [indent ++ "|__" ++  (show expr)]
	make (Transient t) = (indent ++ msg) : (indent ++ "|__" ++ show expr) : (pprintTree (indent ++ " ") "|" t)
	make (Decompose ts) = (indent ++ msg) :  (indent ++ "|__" ++ show expr): (concat (map (pprintTree (indent ++ " ") "|") ts))
	make (Variants cs) = 
		(indent ++ msg) :  (indent ++ "|__" ++  show expr) : (concat (map (\(x, t) -> pprintTree (indent ++ " ") ("?" ++ show x) t) cs))