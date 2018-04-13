#######
# FDA #
#######

if(!require(earth)) install.packages("earth")
if(!require(mda)) install.packages("mda")

library(earth)
library(mda)

fdaGrid <- expand.grid(
  nprune=c(50),
  degree=2
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

fda <- train(
  x=train.x.bin,
  y=train.y$FlagAIB,  
  method="fda",
  trControl=fitControl,
  tuneGrid=fdaGrid,
  metric="ROC"
)

plot(varImp(fda, scale = FALSE), main = "FDA")