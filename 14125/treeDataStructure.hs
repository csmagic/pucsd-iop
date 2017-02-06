data BTree = Leaf Int | Branch Int BTree BTree | EmptyBTree deriving (Show)

dfs :: BTree -> [Int]
dfs EmptyBTree = []
dfs (Leaf a) = [a]
dfs (Branch a ltree rtree)= [a]++ dfs(ltree) ++ dfs(rtree) 

t1 = dfs (Branch 4 (Branch 3 (Leaf 1)(Branch 6 (Leaf 3)(Leaf 8))) (Leaf 0))

bfs :: BTree->[Int]
bfs EmptyBTree = []

--------------------------------------------------------------------------------------------------

data NTree = LeafOfNTree Int | BranchOfNTree Int [NTree] | EmptyNTree deriving (Show)

dfs1 :: NTree -> [Int]
dfs1 EmptyNTree = []
dfs1 (LeafOfNTree a) = [a]
dfs1 (BranchOfNTree a children) = [a] ++ concat(map dfs1 children)
--dfs1 (BranchOfNTree a children) = [a] ++ [y|x<-dfs1(children),y<-dfs1(x)]

t2 = dfs1(BranchOfNTree 4[LeafOfNTree 2,BranchOfNTree 3[LeafOfNTree 1,BranchOfNTree 6[LeafOfNTree 3,LeafOfNTree 8]],LeafOfNTree 0])

------------------------------------------------------------------------------------------------------

data BinaryTree = BranchOfBinaryTree Int BinaryTree BinaryTree | EmptyBinaryTree deriving (Show)

dfs2 :: BinaryTree -> [Int]
dfs2 EmptyBinaryTree = []
dfs2 (BranchOfBinaryTree a ltree rtree) = [a]++dfs2(ltree)++dfs2(rtree)

t3 = dfs2 (BranchOfBinaryTree 4 (BranchOfBinaryTree 3 (BranchOfBinaryTree 1 EmptyBinaryTree EmptyBinaryTree) (BranchOfBinaryTree 6 (BranchOfBinaryTree 3 EmptyBinaryTree EmptyBinaryTree)(BranchOfBinaryTree 8 EmptyBinaryTree EmptyBinaryTree ))) (BranchOfBinaryTree 0 EmptyBinaryTree EmptyBinaryTree))

-----------------------------------------------------------------------------------------------------------
data NryTree = BranchOfNryTree Int [NryTree] | EmptyNryTree deriving (Show)

dfs3 ::NryTree ->[Int]
dfs3 (BranchOfNryTree a childrean) = [a] ++ concat(map dfs3 childrean)

dfs3 BranchOfNTree a children) = [a] ++ [y|x<-dfs1(children),y<-dfs1(x)]
t4 = dfs3(BranchOfNryTree 4[BranchOfNryTree 2 [],BranchOfNryTree 3[BranchOfNryTree 1 [],BranchOfNryTree 6[BranchOfNryTree 3 [],BranchOfNryTree 8 []]],BranchOfNryTree 0 []])

-------------------------------------------------------------------------------------------------------------
