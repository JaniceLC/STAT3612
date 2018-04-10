yx_train_bin <- read.csv("./yx_train_bin.csv", header=TRUE)
library(xgboost)

dtrain <- xgb.DMatrix(data=sparse.model.matrix(FlagAIB~.-1, data=yx_train_bin), label=as.numeric(yx_train_bin$FlagAIB)-1)
paramGrid <- expand.grid(
  eta=c(0.01),
  max_depth=c(5, 6),
  subsample=c(0.5, 0.6),
  colsample_bytree=c(0.8, 0.9)
)
best_param <- list()
best_auc <- 0
best_round <- 
for (i in 1:row(paramGrid)){
  current_param <- as.list(paramGrid[i,])
  history <- xgb.cv(
    data=dtrain,
    params=param,
    early_stopping_rounds=40,
    nround=400,
    verbose=0,
    nfold=5,
    eval_metric="auc",
    objective="binary:logistic",
    booster="gbtree"
  )
  current_round <- history$best_iteration
  current_auc <- history$evaluation_log$test_auc_mean[current_round]
  if(max_auc > best_auc){
    best_param <- current_param
    best_auc <- current_auc
    best_round <- current_round
  }
}

best_auc; best_param; best_round

#param <- list(
#  objective="binary:logistic",
#  eta=0.02,
#  max_depth=6,
#  subsample=0.5,
#  colsample_bytree=0.9,
#  eval_metric="auc"
#) > 400.rounds 0.8789686
