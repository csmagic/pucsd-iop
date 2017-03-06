
-- Lambda Expression is represented here as
-- for ex. (\x -> x) as (L ("x", (Val "x")))
-- for ex. (\x -> x+2) as (L ("x", (E Plus (Var "x") (Val 2))))

data Lambda = L (String, Exp) | A (Lambda, Lambda) deriving (Show, Eq)
data Exp = Le Lambda | Var String | Val Int | E Op Exp Exp deriving (Show, Eq)
data Op = Plus | Minus | Mult deriving (Show, Eq)


-- This is to make lambda expression as it takes bind variable and function for same
-- and creates lambda expression.
abst :: String -> Exp -> Lambda
abst var exp = L (var, exp)


{- A variable x occurs free in an expression E if and only if
(a) E is a variable reference and E is the same as x, or
(b) E is of the form (E1 E2) an x occurs free in E1 or E2, or
(c) E is of the form (lambda (y) E0 ), where y is different from x, and x occurs free in E0 -}

is_free :: String -> Lambda -> Bool
is_free var (L (bind, Val v)) = False 
is_free var (L (bind, Var ref)) = if var == ref then True else False 
is_free var (A (lexp1, lexp2)) = is_free var lexp1 || is_free var lexp2
is_free var (L (bind1, (Le e0))) = var /= bind1 && is_free var e0
is_free var (L (bind, (E op e1 e2))) = var /= bind && is_free_exp var (E op e1 e2)

is_free_in_exp var (Val v) = False
is_free_in_exp var (Var ref) = var == ref
is_free_in_exp var (Le lexp) = is_free var lexp
is_free_exp var (E op e1 e2) = is_free_in_exp var e1 || is_free_in_exp var e2

{-
(a) E is of the form (E1 E2) an x occurs bound in E1 or E2, or
(b) E is of the form (lambda (y) E0), where x occurs bound in E0 , or x and y are the same variables and y occurs free in E0
No variable occurs bound in an expression made up of just a single variable!
-}

is_bound :: String -> Lambda -> Bool
is_bound var (L (bind, (Val v))) = False
is_bound var (L (bind, (Var ref))) = False
is_bound var (L (bind1, (Le e0))) = is_bound var e0 || (var == bind1 && is_free bind1 e0)
is_bound var (A (lexp1, lexp2)) = is_bound var lexp1 || is_bound var lexp2
is_bound var (L (bind, (E op e1 e2))) = var == bind && is_free_exp var (E op e1 e2)


  
