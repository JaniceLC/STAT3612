# data import
url <- c("http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/y_train.csv", 
         "http://www.statsoft.org/wp-content/uploads/2018Stat3612/Project/x_test.csv")
name <- c("x_train.csv", "y_train.csv", "x_test.csv")
for(i in 1:length(url)) download.file(url[i], name[i])
x_train <- read.csv("./x_train.csv", header=TRUE)
y_train <- read.csv("./y_train.csv", header=TRUE)
x_test <- read.csv("./x_test.csv", header=TRUE)

# design matrix
x_train_mat <- model.matrix(~.-1, x_train[, -1])
# matrix of train data
train_mat <- cbind(x_train_mat, y_train[, 2])
colnames(train_mat)[ncol(train_mat)] <- "FlagAIB"
# data frame of train data
train <- data.frame(x_train_mat)
train$FlagAIB <- as.factor(y_train[, 2])
train$Gender <- as.factor(train$Gender)
train$EdMother <- as.ordered(train$EdMother)
train$EdFather <- as.ordered(train$EdFather)
train_reduced <- train[sample(1:nrow(train), floor(nrow(train)/2)), ]

# EDA
# correlation plot
corr <- cor(x_train_mat)
library(ggcorrplot)
ggcorrplot(corr,type="lower", outline.col="white",
           lab=TRUE)
ggcorrplot(corr,type="lower", outline.col="white",
           insig="blank", method="circle")
# barplot
ggplot(train, aes(x=FlagAIB, fill=FlagAIB)) +
  geom_bar(stat="count") +
  geom_label(stat="count", aes(label=..count..), size=5) +
  theme_bw()
# barplot (gender)
ggplot(train, aes(x=Gender, fill=Gender)) + 
  geom_bar(stat="count") +
  geom_label(stat="count", aes(label=..count..), size=5) +
  theme_bw()
ggplot(train, aes(x=Gender, fill=FlagAIB)) +
  geom_bar(stat="count", position="dodge") +
  geom_label(stat="count", aes(label=..count..), size=5) +
  theme_bw()
# barplot (edmother)
ggplot(train, aes(x=EdMother, fill=EdMother)) +
  geom_bar(stat="count") +
  geom_label(stat="count", aes(label=..count..), size=5) +
  theme_bw()
ggplot(train, aes(x=EdMother, fill=FlagAIB)) +
  geom_bar(stat="count", position="dodge") +
  geom_label(stat="count", aes(label=..count..), size=5) +
  theme_bw()
# barplot (edfather)
ggplot(train, aes(x=EdFather, fill=EdFather)) +
  geom_bar(stat="count") +
  geom_label(stat="count", aes(label=..count..), size=5) +
  theme_bw()
ggplot(train, aes(x=EdFather, fill=FlagAIB)) +
  geom_bar(stat="count", position="dodge") +
  geom_label(stat="count", aes(label=..count..), size=5) +
  theme_bw()
# 
ggplot(train, aes(x=EdMother, fill=FlagAIB)) +
  geom_bar(stat="count", position="fill") +
  facet_grid(.~EdFather) +
  theme_bw()
# density (inmotif)
p1 <- ggplot(train, aes(x=InMotif_1, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
p2 <- ggplot(train, aes(x=InMotif_2, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
p3 <- ggplot(train, aes(x=InMotif_3, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
grid.arrange(p1, p2, p3)
# density (exmotif)
p1 <- ggplot(train, aes(x=ExMotif_1, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
p2 <- ggplot(train, aes(x=ExMotif_2, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
p3 <- ggplot(train, aes(x=ExMotif_3, fill=FlagAIB)) +
  geom_density(alpha=0.5) +
  theme_bw()
grid.arrange(p1, p2, p3)


# t-sne
cols <- c("red", "blue")
library(tsne)
ecb = function(x, y){
  plot(x, t='n')
  text(x, labels=train_mat[,24], col=cols[train_mat[,24] +1])
}
tsne <- tsne(train_mat[1:1000,1:23], epoch_callback = ecb, 
                perplexity=10, epoch=5)
library(Rtsne)
tsne <- Rtsne(train_mat[, 1:23], check_duplicates = FALSE, 
              pca = FALSE, perplexity=20, theta=0.5, dims=2)
plot(tsne$Y, t='n')
text(tsne$Y, labels=train_mat[,24], col=cols[train_mat[,24] +1])

# preprocessing
library(caret)
library(dplyr)

## optional
## dimension reduction (pca)
train_pca <- preProcess(select(x_train, -c(StudentID, Region)), 
                        method=c("pca", "nzv"), thresh=0.95)

# random forest (w/ ranger)
levels(train$FlagAIB) <- c("worse", "better")
fit_control <- trainControl(method="cv",
                            number=10,
                            classProbs=TRUE,
                            summaryFunction=twoClassSummary)
rf_grid <- expand.grid(mtry=c(2, 3, 4, 5),
                       splitrule=c("gini", "extratrees"),
                       min.node.size=c(1, 3, 5))
rf <- train(FlagAIB~., data=train, 
            method="rf",
            trControl=fit_control,
            tuneGird=rf_grid,
            metric="ROC")

# gradient boosted machine
fitControl <- trainControl(method="repeatedcv",
                           number=5,
                           repeats=1,
                           classProbs=TRUE,
                           summaryFunction=twoClassSummary)
gbm <- train(FlagAIB~., data=train,
             method="gbm",
             trControl=fitControl,
             verbose=FALSE,
             # specify which metric to optimize
             metric="ROC")
whichTwoPct <- tolerance(gbm$results, metric="ROC",
                         tol=2, maximize=TRUE)
predict(gbm, newdata=data.frame(model.matrix(~.-1, x_test[, -1])), 
        type='prob')
trellis.par.set(caretTheme())
densityplot(gbm, pch="|")

## support vector machine
## high complexity
levels(train_reduced$FlagAIB) <- c("worse", "better")
svm <- train(FlagAIB~., data=train_reduced,
             method="svmRadial",
             trControl=fitControl,
             preProc=c("center", "scale"),
             tuneLength=2,
             metric="ROC")

## extreme gradient boosting
xgb <- train(FlagAIB~., data=train,
             method="xgbTree",
             trControl=fitControl,
             preProc=c("center", "scale"),
             metric="ROC")
trellis.par.set(caretTheme())
densityplot(xgb, pch="|")

# glm elastic net
lgt <- train(FlagAIB~., data=train, 
             method = "glmnet", 
             trControl = fitControl,
             metric = "ROC",
             tuneGrid = expand.grid(alpha = 1,
                                    lambda = seq(0.001,0.1,by = 0.001)))
trellis.par.set(caretTheme())
densityplot(lgt, pch="|")

# multilayer perceptron network with dropout
nndo <- train(FlagAIB~., data=train,
              method="mlpKerasDropout",
              trControl=fitControl,
              metric = "ROC")
# boosting
library(xgboost)
bst <- xgboost(data=x_train_mat, label=as.matrix(y_train[,2]), 
               max.depth=2, eta=0.15, nthread=2, nround=50, objective="binary:logistic")
bst_pred <- predict(bst, x_train_mat)

# logistic regression
library(glmnet)
logit <- cv.glmnet(x_train_mat, as.matrix(as.factor(y_train[,-1])),
                family="binomial", alpha=0.1, nfolds=5)
logit_pred <- predict.cv.glmnet(logit, newx=x_train_mat, type="response", s="lambda.min")
library(pROC)
roc_logit <- roc(y_train[, 2]~logit_pred)
plot(roc_logit, legacy.axes=TRUE)
lines(1-roc_logit$specificities, roc_logit$thresholds, lty=2)
legend("bottomright", legend=c(paste("Logistic AUC ", round(roc_logit$auc, 4), sep=""),
                               "Threshold"), col=1, lty=1:2)
