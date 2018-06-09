library(e1071)

dat = read.csv(file="housing.csv", header=TRUE)
caseT = as.data.frame(dat)
ocean= naiveBayes(ocean_proximity~., data = caseT)
ocean

p =predict(ocean,caseT)

t = table(p,caseT$ocean_proximity)
t

A= naiveBayes(AGE~., data = caseT)
A
p1 =predict(A,caseT)

t1 = table(p1,caseT$AGE)
t1



S= naiveBayes(STAR~., data = caseT)
S
p2 =predict(S,caseT)

t2 = table(p2,caseT$STAR)
t2



P_S= naiveBayes(POPULATION_STATUS~., data = caseT)
P_S
p3 =predict(P_S,caseT)

t3 = table(p3,caseT$POPULATION_STATUS)
t3






