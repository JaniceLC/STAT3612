##########
# glmnet #
##########

if(!require(caret)) install.packages("caret")
if(!require(glmnet)) install.packages("glmnet")

library(caret)
library(glmnet)

# set validation mode
fitControl <- trainControl(
  method="cv",
  number=5,
  classProbs=TRUE,
  summaryFunction=twoClassSummary,
  verboseIter=TRUE
)

glmGrid <- expand.grid(
  alpha=0.9,
  lambda=0
)

levels(train.y$FlagAIB) <- c("B", "A")

glm <- train(
  x=train.x.bin,
  y=train.y$FlagAIB,
  method="glmnet",
  tuneGrid=glmGrid,
  #tuneLength=5,
  trControl=fitControl,
  metric="ROC"
)

plot(varImp(glm), main="GLM")

# > glm$bestTune
# alpha       lambda
# 82 0.03777778 0.0002777778
