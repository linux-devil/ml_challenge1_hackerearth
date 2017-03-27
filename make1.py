f = open('submission.csv')
for i in f:
    z = i.strip().split(",")
    if z[1]=="0":
        print z[0],',',0.00001
    else:
        print z[0],',',z[1]
