
data Exp = Pow | Mul | Add | Less | And deriving (Show, Eq)
data Associativity = L | R | N deriving (Show, Eq)

operator_precedence_list = [('^', 5), ('*', 4), ('+', 3), ('<', 2), ('&', 1)]
operator_associativity_list = [(['*', '+', '&'], L), (['^'], R), (['<'], N)]

precedence :: Char -> Int
precedence operator = alookup_preced operator

associativity :: Char -> Associativity
associativity ch = alookup_asso operator_associativity_list ch

alookup_preced :: Char -> Int
alookup_preced operator = gen_lookup operator_precedence_list operator
gen_lookup ls it  = head [snd item | item <- ls, fst item == it] 
  
alookup_asso :: [([Char],Associativity)] -> Char -> Associativity
alookup_asso ls ch = head [snd item | item <- operator_associativity_list, elem ch (fst item)] 

isOperator :: Char -> Bool  
isOperator c = elem c "^*+<&"

data Action = Shift | Reduce | Error
action_associativity_list = [(R, Shift), (L, Reduce), (N, Error)]

alookup_associativity_action assoc = gen_lookup action_associativity_list assoc 

same_inp_operator ::  Char -> Char -> Action
same_inp_operator inp_op stk_op = alookup_associativity_action (associativity inp_op)

comp_operators :: Char -> Char -> Action
comp_operators inp_op stk_op | precedence inp_op > precedence stk_op = Shift
                             | precedence inp_op < precedence stk_op = Reduce
                             | precedence inp_op == precedence stk_op && associativity inp_op /= associativity stk_op = Error
                             | otherwise = same_inp_operator inp_op stk_op
