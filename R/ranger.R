##########
# ranger #
##########

if(!require(caret)) install.packages("caret")
if(!require(ranger)) install.packages("ranger")

library(caret)
library(ranger)

# set tuning parameters
rfGrid <- expand.grid(
  mtry=c(6, 7, 8),
  splitrule=c("gini"),
  min.node.size=c(3)
)

# set validation mode
fitControl <- trainControl(
  method="cv",
  number=5,
  classProbs=TRUE,
  summaryFunction=twoClassSummary,
  verboseIter=TRUE
)

# caret requires factors with valid names
levels(train.y$FlagAIB) <- c("B", "A")

rf <- train(
  x=train.x.bin,
  y=train.y$FlagAIB,
  method="ranger",
  tuneGrid=rfGrid,
  trControl=fitControl,
  importance="impurity",
  metric="ROC"
)

plot(varImp(rf, scale = FALSE), main = "randomForest")
