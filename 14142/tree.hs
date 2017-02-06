data Tree a =
            Leaf a
            | Branch a (Tree a) (Tree a) deriving (Show)

t1 = (Branch 5 (Branch 3 (Leaf 4) (Leaf 6)) (Branch 2 (Leaf 7) (Leaf 9)))

dfs :: Tree Integer -> [Integer]
dfs (Leaf a) = [a]
dfs (Branch a b c) = a : (dfs b) ++ (dfs c)

bfs :: Tree Integer -> [Integer]
bfs (Leaf a) = [a]
bfs (Branch a b c) = a : concat (map bfs (first b ++ first c ++ (children b) ++ (children c)))
  where
    first (Leaf a) = [(Leaf a)]
    first (Branch a b c) = [(Leaf a)]
    children (Leaf a) = []
    children (Branch a b c) = b : c : []
