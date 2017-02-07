> data AExp = BinOp Op AExp AExp | Val Integer | Var String deriving (Show, Eq)
> data BExp = Gt AExp AExp | Lt AExp AExp | Equ AExp AExp | T | F deriving (Show, Eq)
> data Op = OpAdd | OpSub | OpMult deriving (Show, Eq)
> data Stmt = Assign String AExp | If BExp [Stmt] [Stmt] | While BExp [Stmt] deriving (Show, Eq)
> type State = [(String, Integer)]
>
> opTable = [(OpAdd, (+)), (OpSub, (-)), (OpMult, (*))]
>
> evalAexp :: AExp -> State -> Integer
> evalAexp (Var x) s = x'
>  where
>   Just x' = lookup x s
> evalAexp (Val x) _ = x
> evalAexp (BinOp o e1 e2) s = operatorFunc  (evalAexp e1 s)  (evalAexp e2 s)
>  where
>   Just operatorFunc = lookup o opTable --cos we are assuming no errors
>
> evalBexp :: BExp -> State -> Bool
> evalBexp T _ = True
> evalBexp F _ = False
> evalBexp (Gt e e') s = evalAexp e s > evalAexp e' s
> evalBexp (Lt e e') s = evalAexp e s < evalAexp e' s
> evalBexp (Equ e e') s = evalAexp e s == evalAexp e' s

> interpreter :: [Stmt] -> State -> State
> interpreter [] s = s
> interpreter (Assign x e : ps) s = interpreter ps s'
>  where
>   existingVal = lookup x s
>   foundVar = existingVal /= Nothing
>   s' = if foundVar then s1 ++ [(x, evalAexp e s)] else s ++ [(x, evalAexp e s)]
>    where
>     s1 = filter ((/= x) . fst) s
>
> interpreter (While b ss : ps) s = interpreter ps' s'
>  where
>   s' = if bval then interpreter ss s else s
>   bval = evalBexp b s
>   ps' = if bval then (While b ss : ps) else ps
>
> interpreter (If b ss ss' : ps) s = interpreter ps s'
>  where
>   s' = if bval then interpreter ss s else interpreter ss' s
>   bval = evalBexp b s
