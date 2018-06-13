library(e1071)
dat = read.csv(file="/home/saazi/Desktop/housing.csv", header=TRUE, sep=",")
dat
caseTita = as.data.frame(dat)
status= naiveBayes(ocean_proximity~., data = caseTita)
status

