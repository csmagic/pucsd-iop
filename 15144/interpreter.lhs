-- date : 3 feb 2017
-- code : improvement in interpreter.lhs Introduced Boolean and Arithmatic expression

> import Data.List
>
> data Expr where
>    AExpr :: AExpr -> Expr
>    BExpr :: BExpr -> Expr
>    deriving (Show)
>
> data AExpr where 
>    Variable         :: String -> AExpr
>    Value            :: Int -> AExpr 
>    Plus,Minus,Mult  :: AExpr -> AExpr -> AExpr 
>     deriving (Show)
>
> data BExpr where 
>    Lt,Gt,EqI :: Expr -> Expr -> BExpr
>    BoolVal  :: Bool -> BExpr 
>    deriving (Show)
>
> data Stmt where 
>    Assign :: Var -> Expr -> Stmt 
>    If :: CExpr -> Block -> Block -> Stmt
>    While :: CExpr -> Block -> Stmt
>    deriving (Show)
>
> data Block = Block [Stmt] deriving (Show)
>
> data CExpr where 
>    And,Or :: CExpr -> CExpr -> CExpr
>    Not :: CExpr -> CExpr
>    CExpr :: BExpr -> CExpr 
>    deriving (Show)
> 
> data State = State [(Var,Result)] deriving (Show,Eq)
> data  Var = Var [Char] deriving (Show,Eq)
> data Result = Num Int | Boolean Bool deriving (Show,Eq)
>
> e1 =  AExpr (Plus (Variable "x") (Value 3))
> b1 = BExpr (EqI (e1) (e2))
> e2 = AExpr (Value 6)
> e = BExpr (Lt (e1) (e2))
> a1 = (Assign (Var "x") (e2))
> s1 = State []
> s2 = State [(Var "x",Num 4)]
> b2 = BExpr (EqI (BExpr (BoolVal True)) (BExpr (BoolVal True)))
> ce1 = And (CExpr (BoolVal False)) (CExpr (BoolVal True))
> ce2 =  Not( CExpr (BoolVal True))
> ce3 =   CExpr (BoolVal True)
> blk1= Block [Assign (Var "x") (AExpr (Value 4))]
> blk2= Block [Assign (Var "x") (AExpr (Value 4)),Assign (Var "u") (AExpr (Value 11))]
> stmt1 = If (ce3) (Block [Assign (Var "x") (AExpr (Value 4))]) (Block [Assign (Var "x") (AExpr (Value 8))])
>
> evalAExpr :: AExpr -> State -> Int 
> evalAExpr (Variable x)  s = val
>                           where Num val = getVariableVal (Var x) (s)
> evalAExpr (Value a)     s = a
> evalAExpr (Plus e1 e2)  s = (+) (evalAExpr e1 s) (evalAExpr e2 s)
> evalAExpr (Minus e1 e2) s = (-) (evalAExpr e1 s) (evalAExpr e2 s)
> evalAExpr (Mult e1 e2)  s = (*) (evalAExpr e1 s) (evalAExpr e2 s)
>
> evalBExpr :: BExpr -> State -> Bool
> evalBExpr (BoolVal x)                 (s) = x
> evalBExpr (Gt (AExpr e1) (AExpr e2))  (s) = (>) (evalAExpr e1 s) (evalAExpr e2 s)
> evalBExpr (Lt (AExpr e1) (AExpr e2))  (s) = (<) (evalAExpr e1 s) (evalAExpr e2 s)
> evalBExpr (EqI (AExpr e1) (AExpr e2)) (s) = (==) (evalAExpr e1 s) (evalAExpr e2 s)
>
> -- confusion
> evalCExpr :: CExpr -> State -> Bool
>-- if variable is presend then how to handle that case
>--evalCExpr (CExpr (AExpr (Variable x))) s = val 
>--                                  where Boolean val = getVariableVal (Var x) (s)
> evalCExpr (CExpr (BoolVal x)) s = x
> evalCExpr (And ce1 ce2) s = and([ (evalCExpr ce1 s) ,(evalCExpr ce2 s)])
> evalCExpr (Or ce1 ce2) s = or([ (evalCExpr ce1 s) ,(evalCExpr ce2 s)])
> evalCExpr (Not ce1)   s = not(evalCExpr ce1 s)
>
>
>
> eval :: Expr -> State -> Result
> eval (AExpr x) (s) = Num (evalAExpr x s)
> eval (BExpr x) (s) = Boolean (evalBExpr x s)
>
> getVariableVal :: Var -> State -> Result 
> getVariableVal (x) (State state) = snd.head$filter ((==x).(fst)) state
>
> evalStmt :: Stmt -> State  -> State
> evalStmt (Assign (Var x) (e)) (State s) | (isPresent (Var x) (State s))==True = updateState (Var x,result) (State s)
>                                         | otherwise = State ((Var x,result):s)
>                                         where result = (eval e (State s))
> evalStmt (If c b1 b2) (State s) | (evalCExpr c (State s)) = evalBlock b1 (State s)
>                                 | otherwise = evalBlock b2 (State s)
> evalStmt (While c b1) (State s) | (evalCExpr c (State s)) = evalStmt (While c b1) (evalBlock b1 (State s))
>                                 | otherwise = State s
>
> updateState :: (Var,Result) -> State -> State
> updateState (var,rs) (State s) = State ((var,rs): newState)
>                                 where newState = delete (val) s
>                                       val = (var,snd.head$filter ((==var).(fst)) s)
>
> isPresent :: Var -> State -> Bool
> isPresent var (State currState) = elem var (map fst currState)
>
>
> evalBlock :: Block -> State -> State
> evalBlock (Block [])     s = s
> evalBlock (Block (x:xs)) s = evalBlock (Block xs) (evalStmt x s)
>
> 
>
