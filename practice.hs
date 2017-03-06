
--data Term  = K | S | Term

data Term x where
   K :: Term (a -> b -> a)
   S :: Term ((a -> b -> c) -> (a -> b) -> c) 
   Const :: a -> Term a

data Empty
data NonEmpty
data List x y where
     Nil :: List a Empty
     Cons :: a -> List a b -> List a NonEmpty

data Tree where 
     Lf :: Int -> Tree
     Br :: Int -> Tree -> Tree -> Tree


dfs :: Tree -> [Int]
dfs (Lf x) = [x]
dfs (Br val lc rc) = [val] ++ (dfs lc) ++ (dfs rc)

bfs :: [Tree] -> [Int]
bfs ((Lf x):[]) = [x]
bfs (x:xs) = [ root $ x ] ++ bfs ( xs ++  (children $ x)) 

root :: Tree -> Int
root (Br x _ _) = x
root (Lf x)     = x

children :: Tree -> [Tree]
children (Br val x y) = [x,y]
children (Lf x)       = []

t1 = Br 1 (Lf 2) (Lf 3)
t2 = Br 3 t1 t1

-- naray tree definition 


-- expression evaluator


                                  -- grammer for expression tree






opList   = [(Plus,(+)),(Minus,(-)),(Mult,(*))] 

data Op  = Plus | Minus | Mult deriving (Eq,Show)

data Exp where
  BinaryOp :: Op -> Exp -> Exp -> Exp
  IVal     :: Int -> Exp 
  String1  :: String ->  Exp	
  deriving Show






getOp :: Maybe (a-> a-> a) -> (a -> a -> a) 
getOp (Just x) = x



expEval :: Exp ->[([Char],Int)] -> Int  
expEval (String1 x1 )     ys   = remove1 x1 ys   
expEval (IVal    x  )     ys   = x 
expEval (BinaryOp o x y)  ys   = (getOp $ lookup o opList) (expEval x ys) (expEval y ys)    


remove1 :: [Char] -> [([Char],Int)] -> Int
remove1 x []     = 0
remove1 x (y:ys) = if x == (fst y) then (snd y)
                    else remove1 x ys




                                   -- expression evaluator

data State where
  State1 ::  [([Char],GExp)] -> State
  deriving Show 

data Stmt where 
--  Assign :: String -> Exp -> Stmt
  Assign :: String -> GExp   -> Stmt	
  If     :: BExp   -> [Stmt] -> Stmt
  While  :: BExp   -> [Stmt] -> Stmt       	 

fol1 :: (Stmt -> State -> State) -> State ->[Stmt] -> State
fol1 f s []     = s
fol1 f s (x:xs) = fol1 f (f x s) xs

intpr :: Stmt -> State -> State
intpr (Assign x exp) (State1 y) = State1 ((remove x y) ++ [(x,eval exp (State1 y))]) 
intpr (If x xs) (State1 y)      = if (getGVal  $ eval (B x) (State1 y)) then fol1 intpr  (State1 y) (xs)
                                  else (State1 y)
intpr (While x xs) (State1 y)   = if (getGVal $ eval (B x) (State1 y))  then intpr (While x xs) (fol1 intpr (State1 y) xs)
                                  else (State1 y)


remove :: String -> [([Char],GExp)] -> [([Char],GExp)]
remove x []     = []
remove x (y:ys) = if x == fst y then ys
                  else [y] ++ remove x ys 




                                  -- expression Binary




data BEq = Le   | Lt    | Ge  | Gt deriving (Show,Eq)

data BExp where
  BT    :: Bool -> BExp
  ROpE  :: BEq  -> Exp -> Exp -> BExp 
  ROpB  :: BEq  -> BExp -> BExp -> BExp
  deriving Show	

data GExp where
  B ::  BExp -> GExp
  A ::  Exp  -> GExp
  deriving Show


--                                     Code of GExp

opList1:: Ord a => [(BEq,a->a->Bool)]
opList2   = [(Plus,(+)),(Minus,(-)),(Mult,(*))]
opList1   = [(Le,(<=)),(Lt,(<)),(Gt,(>)),(Ge,(>=))] 



getOp1 :: Maybe (a-> a-> Bool) -> (a -> a -> Bool) 
getOp1 (Just x) = x

getGVal :: GExp -> Bool
getGVal (B (BT x)) = x



getGVal1 :: GExp -> Int
getGVal1 (A (IVal x)) = x

	--                              General Expression Evaluator
eval :: GExp -> State -> GExp
eval (A (String1 x))      (State1 xs)  =  (A (IVal (remove2 x (State1 xs))))
eval (A (IVal y1))        (State1 xs)  =  (A (IVal y1))
eval (B (BT x))           (State1 xs)  =  (B (BT x))
eval (A (BinaryOp o x y)) (State1 xs)  =  (A (IVal ((getOp $ lookup o opList2) (getGVal1 $ (eval (A x) (State1 xs))) (getGVal1 $(eval (A y) (State1 xs))))))    
eval (B (ROpE op1 x1 y1)) (State1 xs)  =  (B (BT ((getOp1  $ lookup op1 opList1) (getGVal1 $ (eval (A x1) (State1 xs))) (getGVal1 $(eval (A y1) (State1 xs))))))    
eval (B (ROpB op1 x1 y1)) (State1 xs)  =  (B (BT ((getOp1  $ lookup op1 opList1) (getGVal1 $ (eval (B x1) (State1 xs))) (getGVal1 $(eval (B y1) (State1 xs))))))    


remove2 :: [Char] -> State -> Int
remove2 x (State1 [])     = 0
remove2 x (State1 ((y,(A (IVal x1))):ys)) = if x == y then x1
                         else remove2 x (State1 ys)
remove2 x (State1 ((y,(B (BT x1))):ys)) = remove2 x (State1 ys)

                          -- compiler and interpreter
data Instr where
  Ldinst :: Int -> Instr
  Mulinst,Plusinst,Minuinst :: Instr
  deriving Show



compiler :: Exp -> [Instr]
compiler (IVal x)                  = [Ldinst x]
compiler (BinaryOp Plus lt rt)     = (compiler lt) ++ (compiler rt) ++ [Plusinst]
compiler (BinaryOp Mult lt rt)     = (compiler lt) ++ (compiler rt) ++ [Mulinst]
compiler (BinaryOp Minus lt rt)    = (compiler lt) ++ (compiler rt) ++ [Minuinst]



type Mem = [Int]
interpreter :: [Instr] -> Mem -> Int
interpreter [] (y:[])                    = y
interpreter ((Ldinst y):xs)  ys          = interpreter xs (y:ys)
interpreter (Mulinst  : xs) ys@(x:y:ps)  = interpreter xs ((x*y):ps)
interpreter (Plusinst  : xs) ys@(x:y:ps) = interpreter xs ((x+y):ps)
interpreter (Minuinst  : xs) ys@(x:y:ps) = interpreter xs ((x-y):ps)

l1 = [Ldinst 1,Ldinst 2,Minuinst,Ldinst 1,Ldinst 2,Minuinst,Minuinst]


