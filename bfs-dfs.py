def dfs(t):
    (val, tl) = t
    if tl == []:
        return [val]
    else :
        return [val] + [x for ll in map (dfs, tl)for x in ll]


    
def bfs(t):
    r= []
    q = [t]
    while(q!=[]):
       (x,ctl) = q[0]
       q = q[1:]+ctl
       r = r +[x]
    return r


def dfsi(t):
    r = []
    s = [t]
    while(s!=[]):
       (x,ctl) = s[0]
       s = ctl+s[1:]
       r = r +[x]
    return r

def bfsp(t):
    return bfs1([t],[])

def bfs1(q,r):
    if (q==[]):
        return r
    else:
        (hd, cl) = q[0]
        tl = q[1:]
        return bfs1(tl+cl, r+[hd])
    
t2= (2,[])
t4= (4,[])
t5= (5,[t2])
t6= (6,[])
t3= (3,[t5,t6])
t1 =(1,[t2, t3,t4])
