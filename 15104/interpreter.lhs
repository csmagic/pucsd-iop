>data AOp   = Plus | Minus | Mul | Div | Mod deriving (Eq , Show)
>data ROp   = Lt | Leq | Gt | Geq | Eql | Neq deriving (Eq , Show)
>data Aexp  = Aexp Aexp AOp Aexp | Val Int | Lit String deriving (Eq , Show)
>data Bexp  = Bexp Bexp ROp Bexp | BExp Aexp ROp Aexp | T | F deriving (Eq , Show)
>data Stmt  = Assign String Aexp | While Bexp [Stmt] | If Bexp [Stmt] [Stmt] deriving (Eq , Show)
>data Inter = Inter Stmt State deriving (Show)
>type State = [(String,Int)]

>opA = [(Plus,(+)) ,(Minus,(-)) ,(Mul,(*)) ,(Div,(div)) ,(Mod,(mod))]
>opR = [(Lt,(<)) ,(Leq,(<=)) ,(Gt,(>)) ,(Geq,(>=)) ,(Eql,(==)) ,(Neq,(/=))]

>getlookupVal :: Maybe a -> a 
>getlookupVal (Just x) = x

>--getlookupRVal :: Maybe (a->a->Bool) -> (a -> a->Bool)
>--getlookupRVal (Just x) = x

>assignmentA :: Aexp -> State -> Int 
>assignmentA (Val x) list = x
>assignmentA (Lit x) list = getlookupVal (lookup x list) 
>assignmentA (Aexp e1 op e2) list = getlookupVal(lookup op opA) (assignmentA e1 list) (assignmentA e2 list)

>assignmentR :: Bexp -> State -> Bool 
>assignmentR (T) list = True
>assignmentR (F) list = False
>assignmentR (Bexp e1 op e2) list =  getlookupVal (lookup op opR) (assignmentR e1 list) (assignmentR e2 list)
>--assignmentR (BExp e1 op e2) list =  getlookupRVal (lookup op opR) (assignmentA e1 list) (assignmentA e1 list) 
>assignmentR (BExp e1 op e2) list | op==Eql = (==) (assignmentA e1 list) (assignmentA e2 list) 
>                                 | op==Neq = (/=) (assignmentA e1 list) (assignmentA e2 list) 
>                                 | op==Lt = (<) (assignmentA e1 list) (assignmentA e2 list) 
>                                 | op==Leq = (<=) (assignmentA e1 list) (assignmentA e2 list) 
>                                 | op==Gt = (>) (assignmentA e1 list) (assignmentA e2 list) 
>                                 | op==Geq = (>=) (assignmentA e1 list) (assignmentA e2 list) 

>interprete :: Stmt -> [(String, Int)] -> [(String, Int)]
>interprete (Assign str exp) list
>                                     | out == []  = (str ,assignmentA (exp) (list)):list
>                                     | otherwise = head(out):(filter (\x -> (fst x) /= str) list)
>                                 where
>                                    out = [(x ,assignmentA (exp) list)| (x,y)<-list, x==str]            

>interprete (While exp []) list       = list
>interprete (While exp (l:ls)) list   
>                                     | assignmentR exp list == False = [] 
>                                     | otherwise = interprete (While exp ls) (interprete l list)

>interprete (If exp (l:ls) []) list   = list
>interprete (If exp [] (x:xs)) list   = list
>interprete (If exp (l:ls) (x:xs)) list   
>                                     | ((l:ls) == [] || (x:xs) == []) = []
>                                     | assignmentR exp list == True = interprete (If exp ls (x:xs)) (interprete l list)
>                                     | otherwise = interprete (If exp (l:ls) xs) (interprete x list)
