setwd("~/Github/IF28/TD2")
library(tree)
calif <- read.table("cadata.txt",header=TRUE)
treefit <- tree(log(MedianHouseValue) ~ Longitude+Latitude,data=calif)
summary(treefit)

treefit3 <- tree(log(MedianHouseValue) ~., data=calif)


library(rpart)

data("Forbes2000", package="HSAUR")
Forbes2000 <- subset(Forbes2000, !is.na(profits))

forbes_rpart<-rpart(profits~assets+marketvalue+sales,data=Forbes2000)
print(forbes_rpart)


