##########
# ranger #
##########

if(!require(caret)) install.packages("caret")
if(!require(ranger)) install.packages("ranger")
if(!require(ggplot2)) install.packages("ggplot2")

library(caret)
library(ranger)
library(ggplot2)

# set tuning parameters
rfGrid <- expand.grid(
  mtry=c(8),
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

# plot variable importance
ggplot(varImp(rf, scale = FALSE), main = "randomForest") +
  ggtitle("Ranger")
