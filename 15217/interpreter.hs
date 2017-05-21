data Aexpression = Myexp Aexpression Oper Aexpression | Val Int | Val1 String deriving (Eq,Show)
data Bexpression = Bexp Bexpression Oper1 Bexpression | BAexp Aexpression Oper1 Aexpression| Literal Bool deriving (Eq,Show)
data Stmt = Assignment String Aexpression | While Bexpression [Stmt] | If Bexpression [Stmt] [Stmt]
data Oper = Plus|Minus|Div|Mod|Mul deriving (Eq,Show)
data Oper1 = LT1|GT1|EQ1|GE1|LE1 deriving (Eq,Show)

roperator :: [(Oper1,(Bool->Bool->Bool))]
roperator1 :: [(Oper1,(Int->Int->Bool))]
operator = [(Mul,(*)),(Plus,(+)),(Minus,(-)),(Mod,(rem)),(Div,(div))]
roperator=[(LT1,(<)),(GT1,(>)),(EQ1,(==)),(LE1,(<=)),(GE1,(>=))]
roperator1=[(LT1,(<)),(GT1,(>)),(EQ1,(==)),(LE1,(<=)),(GE1,(>=))]
l (Just a) = a


type State = [(String,Int)]
eval :: Aexpression -> State -> Int
eval (Val x) []= x
eval (Val x) (ls) = x
eval (Val1 x) (ls)= (l(lookup x ls))
eval (Myexp e1 op e2) (ls) = (l(lookup op operator)) (eval e1 ls)(eval e2 ls)

beval :: Bexpression-> State-> Bool
beval (Literal x) [] = x
beval (Literal x) (ls) = x
beval (BAexp e1 op e2) (ls) = (l(lookup op roperator1)) (eval e1 ls)(eval e2 ls)
beval (Bexp e1 op e2) (ls) = (l(lookup op roperator)) (beval e1 ls)(beval e2 ls)




interpreter :: Stmt -> State -> State
interpreter (Assignment x e) [] = [(x,eval e [])]
interpreter (Assignment x e) ((s,a):ls) = if x==s then ((s,eval e ls):ls) else ((s,a):interpreter (Assignment x e) ls)
interpreter (While e y) (ls) = if (beval e (ls)) then (interpreter (While e y) (interpreterlist y (ls))) else ls
interpreter (If b e1 e2) (ls) = if (beval b (ls)) then (interpreterlist e1 (ls)) else (interpreterlist e2 (ls))


interpreterlist :: [Stmt] -> State-> State
interpreterlist [] (ls) = (ls)
interpreterlist (l:ls) (ls1) = interpreterlist ls (interpreter l (ls1)) 

