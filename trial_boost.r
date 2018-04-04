# data import
url <- c("http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/y_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_test.csv")
name <- c("x_train.csv", "y_train.csv", "x_test.csv")
for(i in 1:length(url)) download.file(url[i], name[i])
x_train <- read.csv("./x_train.csv", header=TRUE)
y_train <- read.csv("./y_train.csv", header=TRUE)
x_test <- read.csv("./x_test.csv", header=TRUE)
par(mfrow=c(2,2))
for (i in 2:18){
  hist(x_train[, i], main=colnames(x_train)[i])
}
# Boosting
library(xgboost)
bst <- xgboost(data=as.matrix(x_train[,2:ncol(x_train)]), label=as.matrix(y_train[,2]), 
               max.depth=2, eta=0.1, nthread=2, nround=10, objective="binary:logistic")
bst_pred <- predict(bst, as.matrix(x_train[,2:ncol(x_train)]))
