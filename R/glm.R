##########
# glmnet #
##########

if(!require(glmnet)) install.packages("glmnet")
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
  alpha=c(0.02, 0.03, 0.04),
  lambda=c(0.001, 0.005, 0.01)
)

glm <- train(
  x=train.x.bin,
  y=train.y$FlagAIB,
  method="glmnet",
  tuneGrid=glmGrid,
  trControl=fitControl,
  metric="ROC"
)