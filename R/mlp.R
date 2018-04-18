###########
# MLP SGD #
###########

if(!require(caret)) install.packages("caret")
if(!require(FCNN4R)) install.packages("FCNN4R")

library(caret)
library(FCNN4R)

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
mlp <- train(
  x=train.x.bin,
  y=train.y$FlagAIB,  
  method="mlpSGD",
  trControl=fitControl,
  #tuneGrid=fldaGrid,
  tuneLength=3,
  metric="ROC"
)