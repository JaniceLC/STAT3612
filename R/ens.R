#################
# caretEnsemble #
#################


if(!require(caretEnsemble)) install.packages("caretEnsemble")
library("caretEnsemble")

# specify indexList
# indexList <- createFolds(train.y$FlagAIB, k=5)

# set validation mode
fitControl <- trainControl(
  method="cv",
  number=5,
  # to assemble the ensemble
  savePredictions="final",
  classProbs=TRUE,
  # index=indexList,
  summaryFunction=twoClassSummary,
  verboseIter=TRUE
)


modelList <- list(
  # rf <- caretModelSpec(
  #   method="ranger",
  #   tuneGrid=rfGrid
  # ),
  xgb <- caretModelSpec(
    method="xgbTree",
    tuneGrid=xgbGrid
  ),
  # glm <- caretModelSpec(
  #   method="glmnet",
  #   tuneGrid=glmGrid
  # ),
  gbm <- caretModelSpec(
    method="gbm",
    tuneGrid=gbmGrid
  ),
  mars <- caretModelSpec(
    method="gcvEarth",
    tuneGrid=marsGrid
  )
  # fda <- caretModelSpec(
  #   method="fda",
  #   tuneGrid=fdaGrid
  # )
)

# caret requires factors with valid names
levels(train.y$FlagAIB) <- c("B", "A")

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
    method="cv",
    number=5,
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  )
)
summary(greedy)
plot(greedy)
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
# 0.880815

# predict
pred.y <- predict(greedier, newdata=test.x.bin)
test.y <- data.frame(StudentID=1:length(pred.y), FlagAIB=pred.y)
write.csv(test.y, file="C:/Users/taylorsu/Desktop/y_test.csv", row.names=FALSE)
