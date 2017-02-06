data Op = Plus | Minus | Mult deriving (Show, Eq)
data Exp = V [(Char)] | Val Int | E Op Exp Exp deriving (Show, Eq)
data Boolean = T | F deriving (Show, Eq)
data Bexpr = B Bop Exp Exp | Bl Boolean  deriving (Show, Eq)
data Bop = Eq | Ne | Le | Ge | Gt | Lt deriving (Show, Eq)
data St = Var [Char] Exp deriving (Show, Eq)
data State =  S [([Char], Int)] deriving (Show, Eq)

e1 = E Plus (Val 5) (Val 10)
e2 = E Mult (Val 15) (Val 20)
e3 = E Mult (V "x") (Val 40)
e4 = E Minus (Val 6) (Val 16)
stmt1 = Var "x" e1
stmt2 = Var "y" e2
stmtx = Var "x" e4
stmtls1 = [stmt1, stmt2]
stmtls2 = [stmtx, stmt2]
st1 = S [("x", 50)]
st2 = S [("y", 30)]
st3 = S []

bool_Eval (Bl b) (S ls) = if b == T then True else False
bool_Eval (B bop e1 e2) (S ls)= getComparisonFunc bop (eval e1 (S ls)) (eval e2 (S ls))

execute [] st = st
execute (x : xs) st = execute xs (interprit x st)  

myIf bexpr stmt1 stmt2 st = if (bool_Eval bexpr st) then execute stmt1 st else execute stmt2 st

while bexpr stmt st = if (bool_Eval bexpr st) then while bexpr stmt (execute stmt st) else st

getComparisonFunc bop = head [snd c | c <- bFunctionList, fst (c) == bop]

bFunctionList = [(Eq, (==)), (Ne, (/=)), (Gt, (>)), (Lt, (<)), (Ge, (>=)), (Le, (<=))]

functionList = [(Plus, (+)), (Minus, (-)), (Mult, (*))]

reduce (V x) (S xs)= head [snd v | v <- xs, fst v == x]
reduce (Val x) (S xs)= x

deduct  op = head [snd c | c <- functionList, fst (c) == op]

eval (V c) (S ls) = head [snd v | v <- ls, fst v == c]
eval (Val n) (S ls) = n
eval (E op n m) (S ls) = (deduct op) (reduce n (S ls)) (reduce m (S ls))

bind (Var v e) (S ls) = (v, eval e (S ls))

isExist e (S ls) = elem e [fst var | var <- ls]

replace b [] = []
replace b (x:[]) = if fst x == fst b then [b] else [x]
replace b (x:xs) = if fst x == fst b then b:xs else x: replace b xs

replaceBinding b (S ls) = S (replace b ls)

append_Bind b (S ls) = S (ls ++ [b])
interprit stmt st | isExist (fst (bind stmt st)) st = replaceBinding (bind stmt st) st
              | otherwise = append_Bind (bind stmt st) st

