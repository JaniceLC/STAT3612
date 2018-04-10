###########
# XGBoost #
###########

library(xgboost)
library(Matrix)
library(ggplot2)

# construct DMatrix for XGBoost
#dtrain <- xgb.DMatrix(data=sparse.model.matrix(~.-1, data=train.x.bin),
#                      label=as.numeric(train.y$FlagAIB)-1)

# parameter tuning
paramGrid <- expand.grid(
  eta=c(0.02),
  max_depth=c(6),
  subsample=0.55,
  colsample_bytree=0.7 # randomForest
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
    nround=1000,
    verbose=0,
    nfold=5,
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

# train model based on tuned result
param <- list(
  eta=0.02,
  max_depth=6,
  subsample=0.55,
  colsample_bytree=0.7
)
xgb <- xgboost(
  data=as.matrix(train.x.bin),
  label=as.matrix(train.y$FlagAIB), 
  params=param,
  nround=670,
  verbose=0,
  nfold=5,
  eval_metric="auc",
  objective="binary:logistic",
  booster="gbtree"
)

# plot importance of variable
imp.matrix <- xgb.importance(feature_names=colnames(train.x.bin), model=xgb)
xgb.ggplot.importance(imp.matrix)
xgb.plot.importance(imp.matrix)

# predict
pred.y <- predict(xgb, newdata=as.matrix(test.x.bin))
test.y <- data.frame(StudentID=1:length(pred.y), FlagAIB=pred.y)
write.csv(test.y, file="~/y_test_2_2.csv", row.names=FALSE)
