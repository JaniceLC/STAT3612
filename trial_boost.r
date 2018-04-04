# data import
url <- c("http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/y_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_test.csv")
name <- c("x_train.csv", "y_train.csv", "x_test.csv")
for(i in 1:length(url)) download.file(url[i], name[i])
x_train <- read.csv("./x_train.csv", header=TRUE)
y_train <- read.csv("./y_train.csv", header=TRUE)
x_test <- read.csv("./x_test.csv", header=TRUE)
x_train_mat <- model.matrix(~.-1, x_train[, -1])

# EDA
corr <- cor(x_train)
library(ggcorrplot)
ggcorrplot(corr, colors=c("darkblue", "white", "darkred"))

# boosting
library(xgboost)
bst <- xgboost(data=x_train_mat, label=as.matrix(y_train[,2]), 
               max.depth=2, eta=0.15, nthread=2, nround=50, objective="binary:logistic")
bst_pred <- predict(bst, x_train_mat)

# logistic regression
library(glmnet)
logit <- cv.glmnet(x_train_mat, as.matrix(as.factor(y_train[,-1])),
                family="binomial", alpha=0.1, nfolds=5)
logit_pred <- predict.cv.glmnet(logit, newx=x_train_mat, type="response", s="lambda.min")
library(pROC)
roc_logit <- roc(y_train[, 2]~logit_pred)
plot(roc_logit, legacy.axes=TRUE)
lines(1-roc_logit$specificities, roc_logit$thresholds, lty=2)
legend("bottomright", legend=c(paste("Logistic AUC ", round(roc_logit$auc, 4), sep=""),
                               "Threshold"), col=1, lty=1:2)
