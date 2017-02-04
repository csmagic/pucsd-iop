> data Exp  = Bmap Exp Op Exp | Val Int deriving (Eq , Show)
> data Op   = Plus | Minus | Mul | Div | Mod deriving (Eq , Show)
> data Stmt = Assign String Exp deriving (Eq , Show)
> type State = [(String,Int)]

> ls :: Exp
> ls =(Bmap (Bmap (Val 10) Mul (Val 50)) Div (Val 10))

> op1 :: [(Op,(Int->Int->Int))]
> op1 = [(Plus,(+)),(Minus,(-)),(Mul,(*)),(Div,div),(Mod,rem)]

> lookup1 :: Op -> [(Op,(Int->Int->Int))] -> (Int->Int->Int)
> lookup1 op ((op1,f):ls) = if op1 == op then f else (lookup1 op ls) 

> eval :: Exp -> Int
> eval (Val a)         = a
> eval (Bmap e1 op e2) =(lookup1 op op1) (eval e1) (eval e2)

> interp :: Stmt -> State -> State
> interp (Assign x e) [] = [(x,eval e)]
> interp (Assign x e) ((s,a):ls) = if x == s then ((s,eval e):ls) else ((s,a):interp (Assign x e) ls)
