
def dfs(tree):
    (val,tl)=tree
    if tl == []:
        return [val]
    else:
        return [val]+ [x for y in map (dfs,tl) for x in y]

t2= (2,[])
t4= (4,[])
t5= (5,[t2])
t6= (6,[])
t3= (3,[t5,t6])
t1 =(1,[t2, t3,t4])
print (dfs(t1))
print (t1)
