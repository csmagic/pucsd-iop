data MyType a = Var a
                |Const Int

data MyYetAnotherType f a = YVar (f a)

data Exp = EVar Char | EConst Int

data Colour = Green | Yellow | Blue  

instance Eq Exp  where
   (==) (EVar x)  (EVar y)     = True
   (==) (EConst x) (EConst y)  = True
   (==) _  _ = False

instance (Eq a) => Eq (MyType a) where
     (==) (Var x) (Var y) = x==y
     (==) (Const x) (Const y) = x==y
     (==) _ _ = False 
    
instance Eq Colour where
   Green  == Green  = True
   Yellow == Yellow = True
   Blue   == Blue   = True
   _  == _ = False
