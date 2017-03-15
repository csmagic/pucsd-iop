
data LamExp = Var String | Abs String LamExp | App LamExp LamExp | Const Int | Err String deriving (Eq ,Show)

---------------------------Beta Reduction-------------------------------
--(\x->x)4
----4
--(\y->y)((\x->x)4)
----12
--(\y->x)3
----Not in scope : x

reduction :: String -> LamExp -> LamExp -> LamExp

reduction x (Var y) (Const v) | x==y = (Const v)
                              | otherwise = Err ("Not in scope : "++y)

betaReduction :: LamExp -> LamExp

betaReduction (Const e) = Const e
betaReduction (App (Abs e e1) e2) = reduction e e1 (betaReduction(e2))
betaReduction (Abs e1 e2) = Abs e1 e2
betaReduction (Var x) = Err ("Not in Scope : "++x)

---------------------------Free Variable----------------------------------
freeVar :: LamExp -> Bool

freeVar e = not (free [] e)

free lst (Abs x e) = free (x:lst) e
free lst (App e1 e2) = free lst e1 && free lst e2
free lst (Var x) = hasFree lst x
free lst (Const x) = False 

hasFree [] x = False 
hasFree (y:lst) x = x == y || hasFree lst x
