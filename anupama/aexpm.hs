import qualified Data.Map as Map
import Data.Maybe 

type Env   = Map.Map Char Int
type Error = String

data AExp = Var Char
            | Value Int
            | Plus AExp AExp
            | Mod AExp AExp
            | Mul AExp AExp

aEval :: AExp -> Maybe Int
aEval (Var k)          =  Map.lookup k currEnv 
aEval (Value v)        =  return v
aEval (Plus ax1 ax2)   = (aEval ax1) 
                         >>=
                            (\x -> (aEval ax2) >>= \y -> return(x + y))
aEval (Mul ax1 ax2)    = do
                            x <- aEval ax1
                            y <- aEval ax2
                            return (x * y)

aEval1 :: AExp -> Either Error Int 
aEval1 (Var k)          =  case (Map.lookup k currEnv) of
                               Nothing -> Left $ "unbound variable " ++ [k]
                               Just a  -> return a 
aEval1 (Value v)        =  return v
aEval1 (Plus ax1 ax2)   = (aEval1 ax1) 
                         >>=
                            (\x -> (aEval1 ax2) >>= \y -> return (x + y))
aEval1 (Mul ax1 ax2)    = do
                            x <- aEval1 ax1
                            y <- aEval1 ax2
                            return (x * y)

currEnv = Map.fromList[('X',2),('Y',3),('Z',0)]
