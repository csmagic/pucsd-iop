data Op = Plus | Minus | Mult deriving (Show, Eq)
data Exp = V [(Char)] | Val Int | E Op Exp Exp deriving (Show, Eq)
data Boolean = T | F deriving (Show, Eq)
data Bexpr = B Bop Exp Exp | Bl Boolean  deriving (Show, Eq)
data Bop = Eq | Ne | Le | Ge | Gt | Lt deriving (Show, Eq)
data St = Var [Char] Exp deriving (Show, Eq)
data State =  S [([Char], Int)] deriving (Show, Eq)

beval (Bl b) (S ls) = if b == T then True else False
beval (B bop e1 e2) (S ls)= getComparisonFunc bop (eval e1 (S ls)) (eval e2 (S ls))

execute [] st = st
execute (x : xs) st = execute xs (interp x st)

myIf bexpr stm1 stm2 st = if (beval bexpr st) then execute stm1 st else execute stm2 st
while bexpr stm st = if (beval bexpr st) then while bexpr stm (execute stm st) else st

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
replaceb b [] = []
replace b (x:[]) = if fst x == fst b then [b] else [x]
replace b (x:xs) = if fst x == fst b then b:xs else x: replace b xs
replaceBinding b (S ls) = S (replace b ls)
appendBinding b (S ls) = S (ls ++ [b])
interp stm st | isExist (fst (bind stm st)) st = replaceBinding (bind stm st) st
              | otherwise = appendBinding (bind stm st) st
