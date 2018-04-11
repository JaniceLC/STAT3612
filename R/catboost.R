############
# CatBoost #
############

if(!require(caret)) install.packages("caret")
if(!require(devtools)) install.packages('devtools')
# install CatBoost from github
devtools::install_url('https://github.com/catboost/catboost/releases/
                      /v0.6.1.1/catboost-R-Windows-0.6.1.1.tgz', args = c("--no-multiarch"))

library(caret)
library(catboost)

# set validation mode
fitControl <- trainControl(
  method="cv",
  number=5,
  classProbs=TRUE,
  index=indexList,
  summaryFunction=twoClassSummary,
  savePredictions=TRUE,
  verboseIter=TRUE
)

# set tuning parameters
catGrid <- expand.grid(
  depth=6,
  learning_rate=0.05, #lower
  iterations=700, #higher
  l2_leaf_reg=0.04, #higher
  rsm = 0.9,
  border_count=64
)

# caret requires factors with valid names
levels(train.y$FlagAIB) <- c("B", "A")

cat <- train(
  x=train.x.bin,
  y=train.y$FlagAIB,
  method=catboost.caret,
  logging_level='Silent', 
  preProc=NULL,
  tuneGrid=catGrid,
  trControl=fitControl,
  metric="ROC"
)

importance <- varImp(cat, scale = FALSE)
plot(importance)

save(catboost, file = 'C:\\Users\\User\\Desktop\\cat.rda')
