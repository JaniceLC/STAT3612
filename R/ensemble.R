if(!require(SuperLearner)) install.packages("SuperLearner")
if(!require(caret)) install.packages("caret")
if(!require(ranger)) install.packages("ranger")
if(!require(xgboost)) install.packages("xgboost")
library(SuperLearner)
library(caret)
library(xgboost)
library(ranger)
#categorical response : 0-1 encoding 
set.seed(150)
fit_control <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary,
                            savePredictions=TRUE)
grid <- expand.grid(depth = 6,
                    learning_rate = 0.05, 
                    iterations = 500,
                    l2_leaf_reg = 0.03,
                    rsm = 0.85,
                    border_count = 64)

#prediction = predict.SuperLearner(ensemble, newdata = train.x.bin)


#CONFUSION MATRIX
SL.ranger.tune = function(...){
  SL.ranger(..., 
    mtry = 6,
    splitrule=c("gini"),
    min.node.size = 3
  )
}


SL.xgboost.tune = function(...){
  SL.xgboost(...,
       eta=0.02,
       max_depth=6,
       subsample=0.55,
       colsample_bytree=0.7, 
       nround = 630,
       booster="gbtree",
       objective="binary:logistic"
  )
}

SL.caret.tune = function(...){
  SL.caret(..., 
           method = catboost.caret,
           logging_level = 'Silent', 
           preProc = NULL,
           tuneGrid = grid, 
           trControl = fit_control,
           metric="ROC"
  )
}
xgb.ranger = CV.SuperLearner(as.numeric(train.y$FlagAIB)-1,
                          train.x.bin, 
                          family = binomial(),
                          method = "method.AUC",
                          verbose = T,
                        cvControl = list(V= 10, shuffle = FALSE),
                          SL.library = list("SL.xgboost.tune", "SL.ranger.tune")
)

xgb.ranger.catboost = CV.SuperLearner(as.numeric(train.y$FlagAIB)-1,
                             train.x.bin, 
                             family = binomial(),
                             method = "method.AUC",
                             verbose = T,
                             cvControl = list(V= 10, shuffle = FALSE),
                             SL.library = list("SL.xgboost.tune", "SL.ranger.tune")
)