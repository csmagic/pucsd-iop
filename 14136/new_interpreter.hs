ops = [(Plus, (+)),(Mult, (*))]
state = [('x',1),('y',2),('z',3)]
data Binop = Plus
     	   |Mult
	   |Mod deriving(Show,Eq)

data Expr = Op Binop Expr Expr
     	  | Val Int deriving(Show,Eq)

--mylookup :: a -> [(a,b)] -> b
mylookup x ((a,b):lst) = if x == a then b else mylookup x lst


myeval :: Expr -> Int
myeval (Val x) = x
myeval (Op binop exp1 exp2) = (mylookup binop ops) (myeval exp1) (myeval exp2)


--update_val :: a -> b -> [(a,b)] -> [(a,b)]
update_val x _ [] = []
update_val x y ((a,b):lst) = if x == a then ((x,y):lst) else update_val x y lst

data Ctype = Assign Char Expr deriving (Show)

interpreter :: Ctype -> [(Char,Int)] -> [(Char,Int)] 
interpreter (Assign x exp) lst = update_val x (myeval exp) lst

a = (Op Plus (Val 5) (Val 3))
stmt = Assign 'x' a