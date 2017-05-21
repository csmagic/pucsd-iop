 data Exp   = A Aexp | B Bexp deriving (Eq ,Show) 

> data Aexp  = Aexp Aexp Op1 Aexp | Val Int | Var String deriving (Eq , Show)
> data Bexp  = Bexp Bexp Op2 Bexp | Baexp Aexp Op2 Aexp | T | F deriving (Eq , Show)
> data Op1   = Plus | Minus | Mul | Div | Mod deriving (Eq , Show)
> data Op2   = Lt | Leq | Gt | Geq | Eql | Neq deriving (Eq , Show)
> data Stmt  = Assign String Aexp | While Bexp [Stmt] | If Bexp [Stmt] [Stmt] deriving (Eq , Show)
> type State = [(String,Int)]

> ls :: Aexp
> ls =(Aexp (Var "a") Plus (Val 10))

> op1 :: [(Op1,(Int->Int->Int))]
> op1 = [(Plus,(+)),(Minus,(-)),(Mul,(*)),(Div,div),(Mod,rem)]

> op2 :: [(Op2,(Int->Int->Bool))]
> op2 = [(Lt,(<)),(Leq,(<=)),(Gt,(>)),(Geq,(>=)),(Eql,(==)),(Neq,(/=))]

> op3 :: [(Op2,(Bool->Bool->Bool))]
> op3 = [(Lt,(<)),(Leq,(<=)),(Gt,(>)),(Geq,(>=)),(Eql,(==)),(Neq,(/=))]

> abexp :: (Maybe a) -> (Maybe (a->a->a)) -> (Maybe a) -> (Maybe a)
> abexp (Just a)  (Just op) (Just b) = Just $ op a b
> abexp     _        _        _      = Nothing

> bexp :: (Maybe Int) -> (Maybe (Int->Int->Bool)) -> (Maybe Int) -> (Maybe Bool)
> bexp (Just a)  (Just op) (Just b) = Just $ op a b
> bexp     _        _        _      = Nothing

> evalAexp :: Aexp -> State -> Maybe Int
> evalAexp (Val v)         varList = Just v
> evalAexp (Var v)         varList = lookup v varList
> evalAexp (Aexp e1 op e2) varList = abexp (evalAexp e1 varList) (lookup op op1) (evalAexp e2 varList)

> evalBexp :: Bexp -> State -> Maybe Bool
> evalBexp  T               varList = Just True
> evalBexp  F               varList = Just False
> evalBexp (Bexp e1 op e2)  varList = abexp (evalBexp e1 varList) (lookup op op3) (evalBexp e2 varList)
> evalBexp (Baexp e1 op e2) varList = bexp (evalAexp e1 varList) (lookup op op2) (evalAexp e2 varList)

