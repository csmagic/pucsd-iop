data AirthmeticOperator = Plus | Minus | Mul deriving (Show,Eq)
data RelationalOperator = LessThen | GreaterThen | EqualTo deriving (Show,Eq)
data LogicalOperator = And | Or deriving (Show,Eq)

data State = State [(Variable,Value)] deriving (Show,Eq)
data Variable = Var String deriving (Show,Eq)
data Value = Result Int | Boolean Bool deriving (Show,Eq)
data Ctype = Assign String AirthmeticExp| While RelationalExp [Ctype] deriving (Show,Eq)

data AirthmeticExp  = VarA String | Num Int | AirthmeticOperation AirthmeticOperator AirthmeticExp AirthmeticExp  deriving (Show,Eq)
data RelationalExp = Val Bool | RelationalOperation  RelationalOperator AirthmeticExp AirthmeticExp deriving (Show,Eq)
data LogicalExp = BoolVal Bool | LogicalOperation LogicalOperator RelationalExp RelationalExp | Not RelationalExp deriving (Show,Eq)   

aeval :: Ctype -> State ->State
aeval (Assign v e) [] = State [(Var v,Result (eval e))]
--aeval (Assign v e) ((s,a):ls) = if v==s then ((s,eval e))

eval :: AirthmeticExp -> Int
eval (Num n) = n
eval (AirthmeticOperation Plus e e') = (eval e) + (eval e')
eval (AirthmeticOperation Minus e e') = (eval e) - (eval e')
eval (AirthmeticOperation Mul e e') = (eval e) * (eval e')

reval :: RelationalExp -> Bool
reval (Val n) = n
reval (RelationalOperation LessThen e e') = (eval e) < (eval e')
reval (RelationalOperation GreaterThen e e') = (eval e) > (eval e')
reval (RelationalOperation EqualTo e e') = (eval e) == (eval e')

leval :: LogicalExp -> Bool
leval (BoolVal n) = n
leval (LogicalOperation And e e') = (reval e) && (reval e')
leval (LogicalOperation Or e e') = (reval e) || (reval e')
leval (Not e ) = not (reval e)


ls = (AirthmeticOperation Plus (Num 25) (AirthmeticOperation Mul (Num 34) (Num 456)))
ls1 = (AirthmeticOperation Mul (AirthmeticOperation Minus (Num 26) (Num 12)) (AirthmeticOperation Plus (Num 10)(Num 14)))
ls2 = reval(RelationalOperation LessThen ls ls1)
ls3 = reval(RelationalOperation EqualTo ls ls)
s1 = (State [])
ls4 = (Assign "X" ls) 
 
