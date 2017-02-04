data Operator = Plus | Minus | Mul deriving (Show,Eq)
data RealationalOperator = LessThen | GreaterThen | EqualTo deriving (Show,Eq)
data LogicalOperator = And | Or | Not deriving (Show,Eq)

data AirthmeticExp  = Num Int | AirthmeticOperator Operator AirthmeticExp AirthmeticExp  deriving (Show,Eq)
data BoolExp = Val Bool | BExp  RealationalOperator AirthmeticExp AirthmeticExp deriving (Show,Eq)
--data LogicalExp =    

eval :: AirthmeticExp -> Int
eval (Num n) = n
eval (AirthmeticOperator Plus e e') = (eval e) + (eval e')
eval (AirthmeticOperator Minus e e') = (eval e) - (eval e')
eval (AirthmeticOperator Mul e e') = (eval e) * (eval e')

beval :: BoolExp -> Bool
beval (Val n) = n
beval (BExp LessThen e e') = (eval e) < (eval e')
beval (BExp GreaterThen e e') = (eval e) > (eval e')
beval (BExp EqualTo e e') = (eval e) == (eval e')


ls = (AirthmeticOperator Plus (Num 25) (AirthmeticOperator Mul (Num 34) (Num 456)))
ls1 = (AirthmeticOperator Mul (AirthmeticOperator Minus (Num 26) (Num 12)) (AirthmeticOperator Plus (Num 10)(Num 14)))
ls2 = beval(BExp LessThen ls ls1)
