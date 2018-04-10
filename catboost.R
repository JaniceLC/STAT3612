
library(catboost)
fit_control <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary,
                            savePredictions=TRUE)

grid <- expand.grid(depth = 6,
                    learning_rate = c(0.05, 0.075, 0.1), 
                    iterations = 500,
                    l2_leaf_reg = c(0.02, 0.03, 0.04),
                    rsm = c(0.85, 0.9),
                    border_count = 64)



catboost <- train(yx_train_bin[, -1],  yx_train_bin[,1],
                  method = catboost.caret,
                  logging_level = 'Silent', 
                  preProc = NULL,
                  tuneGrid = grid, 
                  #tuneLength = 5,
                  trControl = fit_control,
                  metric="ROC")

importance <- varImp(catboost, scale = FALSE)
auc_catboost = auc_extract(catboost)

save(catboost, file = 'C:\\Users\\User\\Desktop\\cat.rda')