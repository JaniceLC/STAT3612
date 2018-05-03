###########
# XGBoost #
###########

if(!require(xgboost)) install.packages("xgboost")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ROCR)) install.packages("ROCR")
library(caret)
library(xgboost)
library(Matrix)
library(ggplot2)
library(ROCR)
# construct DMatrix for XGBoost (optional)
xgb.train <- xgb.DMatrix(data=as.matrix(train.x),
                      label=train.y)
xgb.val <- xgb.DMatrix(data=as.matrix(val.x))

# parameter tuning
# package Caret also provides a systematic framework for tuning

param.grid <- expand.grid(
  eta=c(0.01),
  max_depth=c(3),
  subsample=c(0.5),
  colsample_bytree=c(0.95) # randomForest
)

xgb.temp <- xgb.cv(
  data=xgb.train,
  params=param.grid,
  nround=2000,
  verbose=0,
  nfold=5,
  # ---XGBoost documentation---
  # validation error needs to decrease at least every
  # early_stopping_rounds to continue training
  early_stopping_rounds=50,
  eval_metric="auc",
  objective="binary:logistic",
  booster="gbtree"
)

xgb <- xgboost(
  data=xgb.train, 
  params=param.grid,
  nround=xgb.temp$best_iteration,
  verbose=1,
  objective="binary:logistic",
  booster="gbtree"
)

imp.matrix <- xgb.importance(feature_names=colnames(train.x), model=xgb)

library(xgboostExplainer)
explainer = buildExplainer(xgb, xgb.train, type="binary", 
                           base_score=0.5, n_first_tree=xgb$niter-1)
pred.breakdown = explainPredictions(xgb, explainer, xgb.val)

cr <- c("darkblue", "firebrick2")
library(pROC)
pred.y <- predict(xgb, newdata=as.matrix(val.x))
roc(val.y, pred.y)$auc

# DART (Dropout Additive Regression Trees)
# inherits gbtree and has additional parameters
param.grid.dart <- expand.grid(
  eta=c(0.01),
  max_depth=c(3),
  subsample=c(0.5),
  colsample_bytree=c(0.95), # randomForest
  rate_drop=c(0.2),
  skip_drop=c(0.3)
)
best_param <- list()
best_auc <- 0
best_round <- 0
for (i in 1:nrow(paramGrid)){
  current_param <- as.list(param.grid[i,])
  history <- xgb.cv(
    data=xgb.train,
    params=current_param,
    nround=2000,
    verbose=0,
    nfold=5,
    # ---XGBoost documentation---
    # validation error needs to decrease at least every
    # early_stopping_rounds to continue training
    early_stopping_rounds=50,
    eval_metric="auc",
    objective="binary:logistic",
    booster="gbtree",
    sample_type=c("uniform"),
    normalize_type=c("forest")
  )
  current_round <- history$best_iteration
  current_auc <- history$evaluation_log$test_auc_mean[current_round]
  if(current_auc > best_auc){
    best_param <- current_param
    best_auc <- current_auc
    best_round <- current_round
  }
  # make verbose
  print(paste("Round ", i, " completed", sep=""))
}

xgb.dart <- xgboost(
  data=xgb.train,
  params=best_param,
  nround=best_round,
  verbose=0,
  nfold=5,
  eval_metric="auc",
  objective="binary:logistic",
  booster="dart",
  sample_type=c("uniform"),
  normalize_type=c("forest")
)

imp.matrix.dart <- xgb.importance(feature_names=colnames(train.x), model=xgb.dart)
explainer.dart = buildExplainer(xgb.dart, xgb.train, type="binary", 
                                base_score=0.5, n_first_tree=xgb.dart$niter-1)
pred.breakdown.dart = explainPredictions(xgb.dart, explainer.dart, xgb.val)

cr <- c("#66CCFF", "#0066CC")
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(val.x$Didactic_NASIA, pred.breakdown$Didactic_NASIA, 
     col=cr[val.y+1], cex=0.4, pch=16, 
     xlab="Didactic_NASIA", ylab="Didactic_NASIA impact on log-odds")
plot(val.x$Teacher_1, pred.breakdown$Teacher_1, 
     col=cr[val.y+1], cex=0.4, pch=16, 
     xlab="Teacher_1", ylab="Teacher_1 impact on log-odds")
plot(val.x$NumBook, pred.breakdown$NumBook, 
     col=cr[val.y+1], cex=0.4, pch=16, 
     xlab="NumBook", ylab="NumBook impact on log-odds")
plot(val.x$Teacher_2, pred.breakdown$Teacher_2, 
     col=cr[val.y+1], cex=0.4, pch=16, 
     xlab="Teacher_2", ylab="Teacher_2 impact on log-odds")
plot(val.x$Didactic_NASIA, pred.breakdown.dart$Didactic_NASIA, 
     col=cr[val.y+1], cex=0.4, pch=16, 
     xlab="Didactic_NASIA", ylab="Didactic_NASIA impact on log-odds")
plot(val.x$Teacher_1, pred.breakdown.dart$Teacher_1, 
     col=cr[val.y+1], cex=0.4, pch=16, 
     xlab="Teacher_1", ylab="Teacher_1 impact on log-odds")
plot(val.x$NumBook, pred.breakdown.dart$NumBook, 
     col=cr[val.y+1], cex=0.4, pch=16, 
     xlab="NumBook", ylab="NumBook impact on log-odds")
plot(val.x$Teacher_2, pred.breakdown.dart$Teacher_2, 
     col=cr[val.y+1], cex=0.4, pch=16, 
     xlab="Teacher_2", ylab="Teacher_2 impact on log-odds")

xgb.plot.importance(imp.matrix, main ="XGBoost")
xgb.plot.importance(imp.matrix.dart, main ="XGBoost (DART)")
