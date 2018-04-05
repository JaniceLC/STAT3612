# data import
url <- c("http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/y_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_test.csv")
name <- c("x_train.csv", "y_train.csv", "x_test.csv")
for(i in 1:length(url)) download.file(url[i], name[i])
x_train <- read.csv("./x_train.csv", header=TRUE)
y_train <- read.csv("./y_train.csv", header=TRUE)
x_test <- read.csv("./x_test.csv", header=TRUE)

# design *matrix* (excl. labels)
x_train_mat <- model.matrix(~.-1, x_train[, -1])

# *data.frame* of train data (incl. labels)
train <- data.frame(x_train_mat)
train$FlagAIB <- factor(y_train[, 2])

# EDA
corr <- cor(x_train_mat)
library(ggcorrplot)
ggcorrplot(corr,type="lower", outline.col="white",
           lab=TRUE)
ggcorrplot(corr,type="lower", outline.col="white",
           insig="blank", method="circle")

# loading packages
library(caret)
library(dplyr)

## optional
## dimension reduction (pca)
train_pca <- preProcess(select(x_train, -c(StudentID, Region)), 
                            method=c("pca", "nzv"), thresh=0.95)

# random forest (w/ ranger)
library(ranger)
fit_control <- trainControl(method="cv", number=10)
rf_grid <- expand.grid(mtry=c(2, 3, 4, 5),
                       splitrule=c("gini", "extratrees"),
                       min.node.size=c(1, 3, 5))
rf <- train(as.factor(FlagAIB)~., data=train, 
            method="ranger",
            trControl=fit_control,
            tuneGird=rf_grid)

# gradient boosted machine
levels(train$FlagAIB) <- c("worse", "better")
fitControl <- trainControl(method="repeatedcv",
                           number=5,
                           repeats=1,
                           classProbs=TRUE,
                           summaryFunction=twoClassSummary)
gbm <- train(FlagAIB~., data=train,
             method="gbm",
             trControl=fitControl,
             verbose=FALSE,
             # specify which metric to optimize
             metric="ROC")
whichTwoPct <- tolerance(gbm$results, metric="ROC",
                         tol=2, maximize=TRUE)
predict(gbm, newdata=data.frame(model.matrix(~.-1, x_test[, -1])), 
        type='prob')
trellis.par.set(caretTheme())
densityplot(gbm, pch="|")

# support vector machine
svm <- train(FlagAIB~., data=train,
             method="svmRadial",
             trControl=fitControl,
             preProc=c("center", "scale"),
             tuneLength=8,
             metric="ROC")


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
