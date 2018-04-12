###########
# XGBoost #
###########

if(!require(xgboost)) install.packages("xgboost")
if(!require(ggplot2)) install.packages("ggplot2")

library(xgboost)
library(Matrix)
library(ggplot2)

# construct DMatrix for XGBoost (optional)
# dtrain <- xgb.DMatrix(data=sparse.model.matrix(~.-1, data=train.x.bin),
#                       label=as.numeric(train.y$FlagAIB)-1)

# parameter tuning
# package Caret also provides a systematic framework for tuning
paramGrid <- expand.grid(
  eta=c0.015,
  max_depth=6,
  subsample=c(0.5, 0.4, 0.45),
  colsample_bytree=c(0.7, 0.75) # randomForest
)
best_param <- list()
best_auc <- 0
best_round <- 0
for (i in 1:nrow(paramGrid)){
  current_param <- as.list(paramGrid[i,])
  history <- xgb.cv(
    data=as.matrix(train.x.fm.bin),
    label=as.numeric(train.y$FlagAIB)-1,
    params=current_param,
    nround=1000,
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
xgb.fm <- xgboost(
  data=as.matrix(train.x.fm.bin),
  label=as.matrix(train.y$FlagAIB), 
  params=best_param,
  nround=best_round,
  verbose=0,
  nfold=5,
  eval_metric="auc",
  objective="binary:logistic",
  booster="gbtree"
)

# train model based on tuned result
param <- best_param
xgb.fm <- xgboost(
  data=as.matrix(train.x.fm.bin),
  label=as.matrix(train.y$FlagAIB), 
  params=best_param,
  nround=best_round,
  verbose=0,
  nfold=5,
  eval_metric="auc",
  objective="binary:logistic",
  booster="gbtree"
)
imp.matrix.fm <- xgb.importance(feature_names=colnames(train.x.fm.bin), model=xgb.fm)
xgb.ggplot.importance(imp.matrix.fm)
xgb.plot.importance(imp.matrix.fm, main ="xgBoost_fm_Importance")
#best_auc1 =0.8800068, best_param1 eta = 0.01, max_depth= 6, subsample= 0.6, colsample_bytree=0.6
#best_auc2 = 0.8807268, best_param1 eta = 0.015, max_depth= 6, subsample= 0.5, colsample_bytree=0.7

# ---last used tuned parameters---
# param <- list(
#   eta=0.02,
#   max_depth=6,
#   subsample=0.55,
#   colsample_bytree=0.7
# )

# plot variable importance
imp.matrix <- xgb.importance(feature_names=colnames(train.x.bin), model=xgb)
xgb.ggplot.importance(imp.matrix)
xgb.plot.importance(imp.matrix)

# predict
pred.y <- predict(xgb, newdata=as.matrix(test.x.bin))
test.y <- data.frame(StudentID=1:length(pred.y), FlagAIB=pred.y)
write.csv(test.y, file="~/y_test_2.csv", row.names=FALSE)

# DART (Dropout Additive Regression Trees)
# inherits gbtree and has additional parameters
paramGrid <- expand.grid(
  eta=c(0.02),
  max_depth=c(6),
  subsample=0.55,
  colsample_bytree=0.7, # randomForest
  rate_drop=c(0.85),
  skip_drop=c(0.2)
)
best_param <- list()
best_auc <- 0
best_round <- 0
for (i in 1:nrow(paramGrid)){
  current_param <- as.list(paramGrid[i,])
  history <- xgb.cv(
    data=as.matrix(train.x.bin),
    label=as.matrix(train.y$FlagAIB),
    params=current_param,
    nround=800,
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

# tuned DART performs *slightly* better

# for usage of caret
xgbGrid <- expand.grid(
  eta=c(0.02),
  max_depth=c(6),
  subsample=0.55,
  colsample_bytree=0.7, # randomForest
  gamma=0,
  min_child_weight=1,
  nrounds=550
)