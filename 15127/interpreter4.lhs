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

> abexp :: (Either a String) -> (Either (a->a->a) String) -> (Either a String) -> (Either a String)
> abexp (Left val1) (Left op)   (Left val2) = Left $ op val1 val2
> abexp (Right err)     _          _        = Right err
> abexp    _        (Right err)    _        = Right err
> abexp    _           _        (Right err) = Right err

> bexp :: (Either Int String) -> (Either (Int->Int->Bool) String) -> (Either Int String) -> (Either Bool String)
> bexp (Left val1)   (Left op)   (Left val2)  = Left $ op val1 val2
> bexp (Right err)       _           _        = Right err
> bexp    _          (Right err)     _        = Right err
> bexp    _            _         (Right err)  = Right err

> lookup1 :: String -> State  -> Either Int String
> lookup1 val []          = Right$ "Varable '"++val++"' Not Derive"
> lookup1 val ((a,b):ls)  = if (a==val) then (Left b) else (lookup1 val ls)

> lookup2 ::Eq a => a -> [(a,b)]  -> Either b String
> lookup2 val []          = Right $"Operater Not Exit"
> lookup2 val ((a,b):ls)  = if (a==val) then (Left b) else (lookup2 val ls)

> evalAexp :: Aexp -> State -> Either Int String
> evalAexp (Val v)         varList = Left v
> evalAexp (Var v)         varList = (lookup1 v varList)
> evalAexp (Aexp e1 op e2) varList = abexp (evalAexp e1 varList) (lookup2 op op1) (evalAexp e2 varList)

> evalBexp :: Bexp -> State -> Either Bool String
> evalBexp  T               varList = Left True
> evalBexp  F               varList = Left False
> evalBexp (Bexp e1 op e2)  varList = abexp (evalBexp e1 varList) (lookup2 op op3) (evalBexp e2 varList)
> evalBexp (Baexp e1 op e2) varList = bexp (evalAexp e1 varList) (lookup2 op op2) (evalAexp e2 varList)

> interP :: Stmt -> State -> Either State String
> interP (Assign var aexp)              varList = assignInterP var aexp varList [] varList
> interP (While blExp stmtList)         varList = if (isNotError bexp) then ans  else err
>  where
>   bexp = evalBexp blExp varList
>   err  = Right (getValB bexp)
>   ans  = if (getValA bexp) then ans1 else (Left varList)
>    where
>     ans1 = if (isNotError x) then req else (Right (getValB x))
>      where
>       x  = listInterP stmtList varList
>       req  = (interP (While blExp stmtList) (getValA (listInterP stmtList varList)))
> interP (If blExp stmtList1 stmtList2) varList = if (isNotError bexp) then ans  else err
>  where
>   bexp = evalBexp blExp varList
>   err  = Right (getValB bexp)
>   ans  = if (getValA bexp) then ans1 else ans2
>    where
>     ans1 = listInterP stmtList1 varList
>     ans2 = listInterP stmtList2 varList

> isNotError :: Either a b -> Bool
> isNotError (Left a) = True
> isNotError    _     = False

> getValA :: Either a b -> a
> getValA (Left a)  = a

> getValB :: Either a b -> b
> getValB (Right b) = b

> assignInterP :: String -> Aexp -> State -> State -> State -> Either State String
> assignInterP var aexp varList  ds   []             = if (isNotError x) then ans else err 
>  where 
>   x   = evalAexp aexp varList
>   ans = Left (ds++[(var,getValA x)])
>   err = Right (getValB x)
> assignInterP var aexp varList ds ((s,a):rmVarList) = if var == s then (if (isNotError x) then ans else err) else rec
>  where
>   x   = evalAexp aexp varList
>   ans = Left (ds++[(s,getValA x)]++rmVarList)
>   err = Right (getValB x)
>   rec = assignInterP var aexp varList (ds++[(s,a)]) rmVarList

> listInterP :: [Stmt] -> State -> Either State String
> listInterP   []            varList = Left varList
> listInterP (stmt:stmtList) varList =  if (isNotError x) then ans else Right (getValB x)
>  where
>   x   = interP stmt varList
>   err = Right (getValB x)
>   ans = listInterP stmtList (getValA x)
