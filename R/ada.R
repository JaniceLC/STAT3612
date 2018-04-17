############
# AdaBoost #
############

if(!require(ada)) install.packages("ada")
library(ada)

adaGrid <- expand.grid(
  iter=c(500, 750, 1000),
  maxdepth=c(4, 5, 6),
  nu=c(0.01, 0.02, 0.03)
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

ada <- train(
  x=train.x.bin,
  y=train.y$FlagAIB,
  method="ada",
  tuneGrid=adaGrid,
  trControl=fitControl,
  #importance="impurity",
  metric="ROC"
)
plot(ada)

??? 