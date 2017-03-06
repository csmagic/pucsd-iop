
type Variable = Char
data LambdaExp = Var Variable | LambdaAbs Variable LambdaExp | LambdaApp LambdaExp LambdaExp  deriving (Eq, Show)

betaRed :: LambdaExp -> LambdaExp -> LambdaExp
betaRed (LambdaAbs x exp) e = replace exp x e
betaRed exp _ = exp

replace :: LambdaExp -> Variable -> LambdaExp -> LambdaExp
replace (Var v) x e
 | v == x = e
 | otherwise = Var v

replace (LambdaAbs v exp) x e = LambdaAbs v (replace exp x e)
replace (LambdaApp e1 e2) x e = LambdaApp (replace e1 x e) (replace e2 x e)
