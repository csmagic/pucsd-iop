import Data.Char

data Op = Plus | Minus | Mult | Div | Mod | Or | And | Eq | Ge | Le  deriving(Eq , Show)
data Exp = Binary_op Op Exp Exp | Variable Char | Number Integer | Logical_op Op Exp Exp | Not Exp | Bool_val Bool     deriving(Eq,Show)
data Ctype = Assign Char Exp | While Exp [Ctype] | If Exp [Ctype] [Ctype]  deriving(Eq, Show)

statmnt = Assign ('x') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7)))
state = [('x',0),('y',0),('z',0)]
statmnt1 = Logical_op Eq (Number 4) (Number 4)


evaluate (Bool_val y) = Bool_val y
evaluate (Number y)  = Number y
evaluate (Not (Bool_val y)) = Bool_val (not (y))
evaluate (Not x) = evaluate (Not (evaluate (x)))
evaluate (Logical_op x y z) = result x (evaluate y) (evaluate z)
evaluate (Binary_op x y z) = result x (evaluate y) (evaluate z)

result (Plus) (Number x) (Number y) = Number (x + y)
result (Minus) (Number x) (Number y) = Number (x - y)
result (Mult) (Number x) (Number y) = Number (x * y)
result (Div) (Number x) (Number y) = Number (div x y)
result (Or) (Bool_val x) (Bool_val y) = Bool_val (x || y)
result (And) (Bool_val x) (Bool_val y) = Bool_val (x && y)
result (Ge) (Number x) (Number y) = if (x >= y) then (Bool_val True) else (Bool_val False)
result (Le) (Number x) (Number y) = if (y >= x) then (Bool_val True) else (Bool_val False)
result (Eq) (Number x) (Number y) = if (x == y) then (Bool_val True) else (Bool_val False)

list_statemnt1 = [Assign ('y') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7))) , Assign ('x') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7)))
  ]
list_statemnt2 = [Assign ('z') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7))) , Assign ('x') (Binary_op Plus (Number 5) (Binary_op Mult (Number 6) (Number 7)))
  ]

remove (Number x) = x
interpreter (Assign x exp) state =  [if (z == x) then (z,(remove (evaluate exp))) else (z,y) |(z,y) <- state] 
interpreter (While exp list_statemnt) state = if ((evaluate exp) == (Bool_val True)) then update_state list_statemnt state else state
interpreter (If exp list_statemnt list_statemnt2) state = if ((evaluate exp) == (Bool_val True)) then update_state list_statemnt state else update_state list_statemnt2 state 

update_state ([]) state = state
update_state (x:list_statemnt) state = update_state list_statemnt (interpreter x state)


