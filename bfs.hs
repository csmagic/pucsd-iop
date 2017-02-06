data Tree = Lf Int | Br (Int, [Tree]) deriving Show

bfs [] = []
bfs (t : tl) = root t : bfs (tl ++ children t)

root (Lf x) = x
root (Br (x, tl)) = x

children (Lf x) = []
children (Br (x, tl)) = tl
