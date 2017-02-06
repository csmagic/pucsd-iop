--data Operator = Plus|Minus|Mul deriving (show,eq)

data Exp = Num Int | Plus Exp Exp | Minus Exp Exp | Mul Exp Exp| Par Exp |Err String deriving (Show,Eq)

data Tokenize = Tnum Int | Tplus | Tminus | Tmul | Tob | Tcb deriving (Show,Eq)

--evaluate exp = eval . parse . tokenize $ exp

xs1 = "(25+34*456)"
xs2 = "(35+34*(45-6))"
ts1 = tokenize xs1
ts2 = tokenize xs2
evaluate = eval . parse . tokenize $ xs1
-----------------------------------------------------------

eval (Err s) = error s
eval (Num n) = n
eval (Par e) = eval e
eval (Plus e e') = (eval e) + (eval e')
eval (Minus e e') = (eval e) - (eval e')
eval (Mul e e') = (eval e) * (eval e')

tokenize [] = []
tokenize ('(':ls) = Tob : (tokenize ls)
tokenize (')':ls) = Tcb : (tokenize ls)
tokenize ('+':ls) = Tplus : (tokenize ls)
tokenize ('*':ls) = Tmul : (tokenize ls)
tokenize ('-':ls) = Tminus : (tokenize ls)
tokenize ls = t : (tokenize ls')
    where t = Tnum n
          n = toInt str
          str = takeWhile isDigit ls
          ls' = dropWhile isDigit ls

toInt :: String -> Int
toInt ls = sum $ zipWith (*) ds ps
    where ds = map toDigit (reverse ls)
          ps = iterate (*10) 1


isDigit c = filter ((==)c) "0123456789" /=[]

toDigit c = digit
    where ds = zip "0123456789" [0..9]
          digit = snd . head $ (filter (\(x,y)->x==c) ds) ++ [('0',0)]


parse ts = parse' ts []
parse' [] [t] = t 
parse' [] _ = Err"error"
parse' (Tnum n:ts) (Mul (Err"") (Err""):ps) = parse' (Tcb:ts) (Mul(Num n) (Err""):ps)

--parse'(ts) (Par(e):Mul(Err"")(Err""):ps) = parse' (ts) (Mul(Par(e))(Err""):ps)
parse' (Tmul:ts) ( n : Mul (e1) (e2): ps) = parse'(ts) (Mul (Mul (e1) ( n)) (Err""):ps)
parse' (Tplus:ts) ( n : Mul (e1) (e2): ps) = parse'(ts) (Plus (Err"") (Err""): Mul(e1)( n):ps)
parse' (Tplus:ts) ( n :ps) = parse'(ts) (Plus ( n) (Err""):ps)
parse'(Tplus:ts) (Par(e):Mul(Num n)(Err""):ps) = parse'(ts) (  Plus (Mul(Num n)(Par(e))) (Err"") :ps)
parse'(Tmul:ts) (Par(e):Mul(Num n)(Err""):ps) = parse'(ts) (  Mul (Mul(Num n)(Par(e))) (Err"") :ps)
parse'(Tplus:ts) (Par(e):Plus(Num n)(Err""):ps) = parse'(ts) (Plus (Plus(Num n)(Par(e))) (Err"") :ps)
parse'(Tmul:ts) (Par(e):Plus(Num n)(Err""):ps) = parse'(ts) (Mul(Par(e))(Err""):Plus(Num n)(Err""):ps)
parse' (Tplus:ts) ( n : Plus (e1) (e2): ps) = parse'(ts) ((Plus (Plus (e1) ( n))  (Err"")):ps)
parse' (Tmul:ts) ( n : Plus (e1) (e2):ps) = parse'(ts) ( Mul ( n) (Err""):(Plus(e1)(e2)) :ps)
parse' (Tmul:ts) ( n :ps) = parse' (ts) (Mul (n) (Err""):ps)
parse'(Tob:ts) ps = parse' (ts) ((Par(Err"")):ps)
parse' (Tcb:ts) ps = parse' (ts)  (merge ps )
parse' (Tplus:ts) ps = parse' (ts) (Plus (Err"")(Err""):ps)
parse' (Tmul:ts) ps = parse' (ts) (Mul (Err"")(Err""):ps)
parse' (Tnum n :ts) ps = parse' (ts) (Num n :ps)

merge ts = merge' ts 
merge' [t] = [t]
merge' (Mul (e) (Err""): n:ps )  =  (Mul (e) (n) : ps)
merge'(e:Mul(Err"") (Err""):e1:ps) = merge' (Mul (e) (e1):ps)
merge'(e:Plus(Err"")(Err""):e1:ps) = merge' (Plus (e) (e1):ps)
merge' ( n : Plus(e1)(Err""):ts)   = merge' ((Plus (e1) ( n)):ts)
merge'(Par(e): Mul (e1) (Err""):ts) = merge' (Mul(e1)(Par(e)):ts)
merge'(Par(e): Plus (e1) (Err""):ts) = merge' (Plus(e1)(Par(e)):ts)
merge' (ts : Par (Err""):y')  =  (Par(ts):y')

----merge'(Mul(x)(y):ps) = merge' (Mul(x)(y):ps)
----merge'(Plus(x)(y):ps) = merge' (Plus(x)(y):ps)
--
merge' ( n : Mul(e1)(Err"") : ts )   = merge' (Mul (e1) ( n) : ts)
--
----merge' (Mul(e1)(e2) : Plus (e) (Err"") :ts)  = merge' ((Plus (e) (Mul (e1) (e2))):ts)
----merge' (Plus(e1)(e2): A)
----parse :: [AToken] -> Aexp
----parse a = Err ""
---- \x-> fst x == y ====== (==y)(fst x)
-- lookup :: a->[(a,b)] -> Maybe b
