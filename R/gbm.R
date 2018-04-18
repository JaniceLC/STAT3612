#######
# GBM #
#######

if(!require(caret)) install.packages("caret")
if(!require(gbm)) install.packages("gbm")

library(caret)
library(gbm)

gbmGrid <- expand.grid(
  n.trees=c(600),
  interaction.depth=c(9),
  shrinkage=c(0.01),
  n.minobsinnode=c(10)
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
gbm <- train(
  x=train.x.bin,
  y=train.y$FlagAIB,
  method="gbm",
  tuneGrid=gbmGrid,
  trControl=fitControl,
  metric="ROC"
)
plot(gbm)
plot(varImp())
