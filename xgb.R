yx_train_bin <- read.csv("C:/Users/User/Downloads/yx_train_bin.csv", header=TRUE)
library(xgboost)

dtrain <- xgb.DMatrix(data=sparse.model.matrix(FlagAIB~.-1, data=yx_train_bin), label=as.numeric(yx_train_bin$FlagAIB)-1)
paramGrid <- expand.grid(
  eta=0.02,
  max_depth=6,
  subsample=c(0.5, 0.55, 0.6),
  colsample_bytree=0.7 # randomForest
)
best_param <- list()
best_auc <- 0
best_round <- 0
  for (i in 1:nrow(paramGrid)){
    current_param <- as.list(paramGrid[i,])
    history <- xgb.cv(
      data=dtrain,
      params=current_param,
      early_stopping_rounds=40,
      nround=500,
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
    print(paste("Round ", i, " completed", sep=""))
  }

best_auc; best_param; best_round

# > best_auc; best_param; best_round
# [1] 0.8809792
# $eta
# [1] 0.02
# 
# $max_depth
# [1] 6
# 
# $subsample
# [1] 0.55
# 
# $colsample_bytree
# [1] 0.7
# 
# attr(,"out.attrs")
# attr(,"out.attrs")$dim
# eta        max_depth        subsample colsample_bytree 
# 1                1                3                1 
# 
# attr(,"out.attrs")$dimnames
# attr(,"out.attrs")$dimnames$eta
# [1] "eta=0.02"
# 
# attr(,"out.attrs")$dimnames$max_depth
# [1] "max_depth=6"
# 
# attr(,"out.attrs")$dimnames$subsample
# [1] "subsample=0.50" "subsample=0.55" "subsample=0.60"
# 
# attr(,"out.attrs")$dimnames$colsample_bytree
# [1] "colsample_bytree=0.7"
# 
# 
# [1] 490

#param <- list(
#  objective="binary:logistic",
#  eta=0.02,
#  max_depth=6,
#  subsample=0.5,
#  colsample_bytree=0.9,
#  eval_metric="auc"
#) > 400.rounds 0.8789686
