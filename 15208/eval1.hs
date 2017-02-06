data Expression = Exp Expression Opr Expression | Val Int deriving (Eq,Show)
data Opr = Plus | Minus | Mul | Div | Mod deriving (Eq,Show)
data Statement = Assignment String Expression deriving (Eq,Show)
type State =[(String,Int)]

oplist :: [(Opr,(Int->Int->Int))]
oplist = [(Plus,(+)),(Minus,(-)),(Mul,(*)),(Div,div),(Mod,rem)]

look (Just x) = x

evaluate :: Expression -> Int
evaluate (Val a) = a
evaluate (Exp exp1 opr exp2) = (look (lookup opr oplist )) (evaluate exp1) (evaluate exp2)

interprete :: Statement ->State -> State 
interprete (Assignment str exp) [] = [(str,evaluate exp)]
interprete (Assignment str exp) ((str1,ov):remain) = if str == str1 then ((str1 , evaluate exp): remain) else ((str,ov) :interprete (Assignment str exp) remain)
