data Var = V [Char] deriving (Show, Eq)
data Op = Plus | Minus | Mult deriving (Show, Eq)
data Exp = Val Int | Var | E Op Exp Exp deriving (Show, Eq)

-- lambda data type
-- for (\x->x) we have to write (L (V "x", (Val "x")))
-- and for (\x->x*5) we write (L (V "x", (E Mult (val "x") (val "5"))))
data Lambda = L (Var,Exp) | A (Lambda, Lambda) deriving (Show, Eq)

-- bind and create lambda expression 
abst :: Var -> Exp -> Lambda
abst var exp = L(var, exp)

ex = abst (V "x") (E Plus (Val 5) (Val 10))
