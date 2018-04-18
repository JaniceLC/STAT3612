########
# MARS #
########

if(!require(caret)) install.packages("caret")
if(!require(earth)) install.packages("earth")
if(!require(mda)) install.packages("mda")

library(caret)
library(earth)
library(mda)

marsGrid <- expand.grid(
  degree=c(4)
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

mars <- train(
  x=train.x.bin,
  y=train.y$FlagAIB,  
  method="gcvEarth",
  trControl=fitControl,
  tuneGrid=marsGrid,
  metric="ROC"
)

plot(varImp(mars, scale = FALSE), main = "MARS")
