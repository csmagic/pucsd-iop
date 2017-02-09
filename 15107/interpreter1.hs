--- by using Either:


import Data.Char
import Data.Data

data Aop = Plus | Minus | Mult deriving(Eq, Show)
data Cop = Lt | Leq | Gt | Geq | Eql  deriving(Eq , Show)
data Bop = Or | And | Xor deriving(Eq ,Show)
data Aexp = Binary_op Aop Aexp Aexp | Val Integer | Var Char deriving(Eq ,Show)
data Bexp = Com_op Cop Aexp Aexp | Log_op Bop Bexp Bexp | Not Bexp | Bool_val Bool deriving(Eq,Show)
data Ctype = Assign Char Aexp | While Bexp [Ctype] | If Bexp [Ctype] [Ctype] deriving (Eq,Show) 

lookValue :: [(Char,Integer)] -> Char -> (Either [Char] Integer)
lookValue [] x = Left ([x] ++ " is not defined in the list of variables")
lookValue ((y,z):ls) x = if (y == x) then (Right z) else lookValue ls x

---- Arithmatic Expression evaluator ----
lookOp :: (Eq a) => [(a,t)] -> a -> t
lookOp ((y,z):ls) x = if (x == y) then z else lookOp ls x

listOp = [ (Plus ,(+)) ,(Minus , (-)) , (Mult ,(*))  ]

evalArith :: Aexp -> [(Char,Integer)] -> (Either [Char] Integer)
evalArith (Val x) state = Right x
evalArith (Var x) state = (lookValue state x)
evalArith (Binary_op arithOp exp1 exp2) state = result arithOp (evalArith exp1 state) (evalArith exp2 state)  -- (evaluate y state) (evaluate z state)

result arithOp (Right x) (Right y) = Right ((lookOp listOp arithOp)  x y)
result arithOp (Left x) (Left y) = Left (x ++ " and "  ++y)
result arithOp (Left x) (_) = Left (x)
result arithOp (_) (Left y) = Left (y)

----- Bool Expression evaluator ---
lookBop :: (Eq a) => [(a,t)] -> a -> t
lookBop ((y,z):ls) x = if (x == y) then z else lookBop ls x
listBop = [(Eql , (==)) , (Lt , (<)) , (Leq , (<=)) , (Gt , (>)) , (Geq ,(>=))]

evalBool :: Bexp -> [(Char,Integer)] -> (Either [Char] Bool)
evalBool (Bool_val x) state = Right x 
evalBool (Com_op x exp1 exp2) state = resultBool x (evalArith exp1 state) (evalArith exp2 state) 

resultBool boolOp (Right x) (Right y) = Right ((lookBop listBop boolOp) x y)
resultBool boolOp (Left x) (Left y) = Left (x ++ " and " ++ y) 
resultBool boolOp (Left x) (_) = Left x
resultBool boolOp (_) (Left x) = Left x
				

interpreter :: Ctype -> [(Char,Integer)] -> (Either [Char] [(Char,Integer)])
interpreter (Assign x arithexp) state = updateState (evalArith arithexp state) x state
interpreter (While boolexp ls) state = updateWhile (evalBool boolexp state) ls state
interpreter (If boolexp ls1 ls2) state = updateIf (evalBool boolexp state) ls1 ls2 state
 
-- for assign statement --
updateState :: (Either [Char] Integer) -> Char -> [(Char,Integer)] -> (Either [Char] [(Char,Integer)]) 
updateState (Left x) var state = Left x
updateState (Right y) var state = Right [if (x == var) then (x,y) else (x,z) |(x,z) <- state]  

-- for while statement --
updateWhile :: (Either [Char] Bool) -> [Ctype] -> [(Char,Integer)] -> (Either [Char] [(Char,Integer)])
updateWhile (Left x) (_) (_) = Left x
updateWhile (Right False) ls state = Right state 	
updateWhile (Right True) ls state = update_state ls (Right state)

-- for If statement --

updateIf :: (Either [Char] Bool) -> [Ctype] -> [Ctype] -> [(Char,Integer)] -> (Either [Char] [(Char,Integer)]) 
updateIf (Left x) (_) (_) (_) = Left x
updateIf (Right False) ls1 ls2 state = update_state ls2 (Right state)
updateIf (Right True) ls1 ls2 state = update_state ls1 (Right state)

update_state :: [Ctype] -> (Either [Char] [(Char,Integer)]) -> (Either [Char] [(Char,Integer)])
update_state ls (Left x) = Left x
update_state ([]) state = state
update_state (x:list_statemnt) (Right state) = update_state list_statemnt (interpreter x state)



