#################
# caretEnsemble #
#################


if(!require(caretEnsemble)) install.packages("caretEnsemble")
library("caretEnsemble")

# set validation mode
fitControl <- trainControl(
  method="cv",
  number=5,
  # to assemble the ensemble
  savePredictions="final",
  classProbs=TRUE,
  index=indexList,
  summaryFunction=twoClassSummary,
  verboseIter=TRUE
)

# specify indexList
indexList <- createFolds(train.y$FlagAIB, k=5)


modelList <- list(
  rf <- caretModelSpec(
    method="ranger",
    tuneGrid=rfGrid
  ),
  xgb <- caretModelSpec(
    method="xgbTree",
    tuneGrid=xgbGrid
  ),
  glm <- caretModelSpec(
    method="glmnet",
    tuneGrid=glmGrid
  )
)

ensembleList <- caretList(
  x=train.x.bin,
  y=train.y$FlagAIB,
  trControl=fitControl,
  metric="ROC",
  tuneList=modelList
)

# ensembleList[["cat"]] <- cat

xyplot(resamples(ensembleList))

modelCor(resamples(ensembleList))

greedy <- caretEnsemble(
  ensembleList,
  metric="ROC",
  trControl=trainControl(
    number=2,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  )
)
summary(greedy)
greedier <- caretStack(
  ensembleList,
  method="glm",
  metric="ROC",
  # do NOT use the train control object used above
  trControl=trainControl(
    method="boot",
    number=10,
    savePredictions="final",
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  )
)
summary(greedier)