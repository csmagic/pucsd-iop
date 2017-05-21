data Exp = Plus Exp Exp
         | Mult Exp Exp
         | Val Int
         | Var String
data Instr = Ld Int
           | MulI
           | PlusI deriving (Show)
data Boolean = T | F deriving (Show, Eq)
data Bexp = Eq Exp Exp | Lt Exp Exp | Gt Exp Exp | Le Exp Exp | Ge Exp Exp | Ne Exp Exp | Bl Boolean

getVal :: String -> [(String, Int)] -> Int
getVal s l = head (map (\(p, q) -> q) (filter (\(x, y) -> (x == s)) l))

eval :: Exp -> [(String, Int)] -> Int
eval (Val x) st = x
eval (Var x) st = snd (head (filter (\y -> fst y == x) st))
eval (Mult e1 e2) st = (eval e1 st) * (eval e2 st)
eval (Plus e1 e2) st = (eval e1 st) + (eval e2 st)

compile :: Exp -> [Instr]
compile (Val x) = [Ld x]
compile (Mult e1 e2) = compile e1 ++ compile e2 ++ [MulI]
compile (Plus e1 e2) = compile e1 ++ compile e2 ++ [PlusI]

execsm :: [Instr] -> [Int] -> Int
execsm [] (x:s) = x
execsm ((Ld x):l) s = execsm l (x:s)
execsm ((MulI):l) (x:y:s) = execsm l ((x * y):s)
execsm ((PlusI):l) (x:y:s) = execsm l ((x + y):s)

assign :: String -> Exp -> [(String, Int)] -> (String, Int)
assign x e st = (x, eval e st)

interpreter :: (String, Int) -> [(String, Int)] -> [(String, Int)]
interpreter stmt state = stmt : filter (\(x, y) -> x /= (fst stmt)) state

beval :: Bexp -> [(String, Int)] -> Boolean
beval (Bl b) st = b
beval (Eq e1 e2) st = if (eval e1 st) == (eval e2 st) then T else F
beval (Lt e1 e2) st = if (eval e1 st) < (eval e2 st) then T else F
beval (Gt e1 e2) st = if (eval e1 st) > (eval e2 st) then T else F
beval (Le e1 e2) st = if (eval e1 st) <= (eval e2 st) then T else F
beval (Ge e1 e2) st = if (eval e1 st) >= (eval e2 st) then T else F
beval (Ne e1 e2) st = if (eval e1 st) /= (eval e2 st) then T else F

while :: Bexp -> [(String, Exp)] -> [(String, Int)] -> [(String, Int)]
while be sts st = if (beval be st) == T then while be sts (merge (map (\x -> (assign (fst x) (snd x) st)) sts) st) else st
    where
        merge :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
        merge [] old = old
        merge (x : new) old = merge new (interpreter x old)

myif :: Bexp -> [(String,Exp)] -> [(String, Exp)] -> [(String, Int)] -> [(String, Int)]
myif be sts1 sts2 st = if (beval be st) == T then merge (map (\x -> (assign (fst x) (snd x) st)) sts1) st
                                             else merge (map (\x -> (assign (fst x) (snd x) st)) sts2) st
    where
        merge :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
        merge [] old = old
        merge (x : new) old = merge new (interpreter x old)
