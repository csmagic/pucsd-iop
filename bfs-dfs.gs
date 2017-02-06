ctype Tree where
  Lf: Int -> Tree
  Br: (Int, Tree , Tree) -> Tree


dfs. (Lf. x) = [x]
dfs. (Br.(x,t1,t2)) = x :: (dfs.t1 ++ dfs.t2)

-- whats the problem with the commented version?
-- shape mismatch of data recursion and problem recursion!
-- bfs. (Lf.x) = [x]
-- bfs. p@(Br.(x,t1,t2)) = [x]++ map.root.t1 ++ map.root.t2 ++ map.bfs.(children.p)

--Intuition: we need the bfs call outside the children list, in order to go
--breadth and not depth, so I give it a list argument.
-- In the process I have generalised solution to list of trees...

bfs.[] =[]
bfs.(t1::tl) = [root.t1] ++bfs.(tl ++ children.t1)


root.(Lf.x) = x
root.(Br.(x,t1,t2)) = x

children.(Lf.x) = []
children.(Br.(x,t1,t2)) = [t1,t2]


---------N-ary tree 
ctype Tree1 where
     Nt: (Int, [Tree1]) -> Tree1


t1 = Nt.(1, [t2,t3,t4])
t2 = Nt.(2, [t5])
t5 = Nt.(5,[])
t3 = Nt.(3, [Nt.(6,[]), Nt.(7,[])])
t4 = Nt.(4, [Nt.(8,[]), Nt.(9,[])])

dfs1. (Nt.(x,l)) = [x] ++ [y | t <- l,  y <- dfs1.t]
bfs1.[] = []
bfs1. ((Nt.(x,l))::tl) = [x] ++bfs1.(tl++ l)