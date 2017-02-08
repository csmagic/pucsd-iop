import qualified Data.Map as Map
data ArithExp = Const Int   | Var Char | Add ArithExp ArithExp | Mul ArithExp ArithExp

type Env = [(Char,Int)]
expEval :: ArithExp -> Env -> Maybe Int

alookup :: Char -> Env -> Maybe Int
alookup e [] = Nothing
alookup e ((a,b):xs) | e==a = Just b
       	     | otherwise = alookup e xs

expEval (Const x) env = Just x
expEval (Var x) env = alookup x env
expEval (Add exp1 exp2) env = (expEval exp1 env) `aplus` (expEval exp2 env) 
expEval (Mul exp1 exp2) env = (expEval exp1 env) `amul` (expEval exp2 env) 

aplus :: Maybe Int -> Maybe Int -> Maybe Int
aplus Nothing Nothing = Nothing
aplus (Just x) (Just y) = Just (x+y)
aplus (Nothing) (Just x) = Nothing
aplus (Just y) Nothing = Nothing

amul :: Maybe Int -> Maybe Int -> Maybe Int
amul Nothing Nothing = Nothing 
amul (Just x) (Just y) = Just (x*y)
amul (Just x) Nothing = Nothing
amul Nothing (Just x) = Nothing

values = [('x',2),('y',4)] :: Env
