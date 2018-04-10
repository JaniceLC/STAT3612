yx_train_bin <- read.csv("./yx_train_bin.csv", header=TRUE)
library(xgboost)
dtrain <- xgb.DMatrix(data=sparse.model.matrix(FlagAIB~.-1, data=yx_train_bin), label=as.numeric(yx_train_bin$FlagAIB)-1)
paramGrid <- expand.grid(
  eta=c(0.01),
  max_depth=c(5, 6),
  subsample=c(0.5, 0.6),
  colsample_bytree=c(0.8, 0.9)
)
param <- as.list(paramGrid[1,])

#param <- list(
#  objective="binary:logistic",
#  eta=0.02,
#  max_depth=6,
#  subsample=0.5,
#  colsample_bytree=0.9,
#  eval_metric="auc"
#) > 400.rounds 0.8789686

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
print(history)
history$evaluation_log[history$best_ntreelimit]
