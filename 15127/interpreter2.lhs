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

> toVal :: Maybe a -> a
> toVal (Just val) = val


> evalAexp :: Aexp -> State -> Int
> evalAexp (Val v) varList         = v
> evalAexp (Var v) varList         = toVal (lookup v varList)
> evalAexp (Aexp e1 op e2) varList = (toVal (lookup op op1)) (evalAexp e1 varList) (evalAexp e2 varList)

> evalBexp :: Bexp -> State -> Bool
> evalBexp  T varList               = True
> evalBexp  F varList               = False
> evalBexp (Bexp e1 op e2) varList  = (toVal (lookup op op3)) (evalBexp e1 varList) (evalBexp e2 varList)
> evalBexp (Baexp e1 op e2) varList = (toVal (lookup op op2)) (evalAexp e1 varList) (evalAexp e2 varList)

> interP :: Stmt -> State -> State
> interP (Assign var aexp)              varList = assignInterP var aexp varList varList
> interP (While blExp stmtList)         varList = if (evalBexp blExp varList) then (interP (While blExp stmtList) (listInterP stmtList varList)) else varList
> interP (If blExp stmtList1 stmtList2) varList = if (evalBexp blExp varList) then (listInterP stmtList1 varList) else (listInterP stmtList2 varList)

> assignInterP :: String -> Aexp -> State -> State -> State
> assignInterP var aexp varList  []               = [(var,evalAexp aexp varList)]
> assignInterP var aexp varList ((s,a):rmVarList) = if var == s then ((s,evalAexp aexp varList):rmVarList) else ((s,a):assignInterP var aexp varList rmVarList)

> listInterP :: [Stmt] -> State -> State
> listInterP   []            varList =  varList
> listInterP (stmt:stmtList) varList =  listInterP stmtList (interP stmt varList)
