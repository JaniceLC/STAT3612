get.auc = function(model, x, y){
  a = predict.glm(model, newdata = x, type ="response")
  auc = roc(y, a)$auc
  return(auc)
  }

# logistic regression
library(MASS)
library(tikzDevice)
library(pROC)
model1.1 <- glm(FlagAIB~1, data=train.bin, family="binomial")
model1.1.ns <- glm(FlagAIB~1, data = train.ns, family = "binomial")

model1.2 <- glm(FlagAIB~., data=train.bin, family="binomial")
pred1.2 <- predict.glm(model1.2, newdata=val.x.bin, type="response")
(auc1.2 <- roc(val.y, pred1.2)$auc)
#AUC_train = 0.8847
#AUC_test = 0.8554

model1.2.ns <- glm(FlagAIB~., data = train.ns, family = "binomial")
(get.auc(model1.2.ns, train.x.ns, train.y.ns))
(get.auc(model1.2.ns, val.x.ns, val.y))
#AUC_train = 0.8765
#AUC_test = 0.8603

#Performs stepwise model selection by AIC
model1.3 <- stepAIC(model1.1, direction="forward",
                    scope=list(upper=model1.2, lower=model1.1))
summary(model1.3)
 

model1.3.ns <- stepAIC(model1.1.ns, direction="forward",
                    scope=list(upper=model1.2.ns, lower=model1.1.ns))
summary(model1.3.ns)


tikz('/Users/l-c/Desktop/logistic/logistic.tex', width = 8, height = 6)
par(mfrow=c(2,2))
plot(model1.3.ns, pch=19, cex=0.5)
dev.off()




# lasso
library(glmnet)
dummies <- model.matrix(FlagAIB~., data=train.bin)
model2 <- cv.glmnet(x=dummies[, -1], y=train.y.bin, family="binomial", alpha=1, nfolds=5)

tikz('~/lasso.tex', width = 4, height = 8)
par(mfrow=c(1,2))
plot(model2)
plot(model2$glmnet.fit)
dev.off()
val.bin = cbind(FlagAIB = val.y, val.x.bin)
dummies.val <- model.matrix(FlagAIB~., data=val.bin)
pred2 <- predict.cv.glmnet(model2, newx=dummies.val[,-1],
                           s="lambda.1se", type="response")
(auc2 <- roc(val.y, as.numeric(pred2))$auc)
#AUC_TRAIN = 0.883
# AUC_VAL=  0.8553

###with natural spline 

dummies <- model.matrix(FlagAIB~., data=train.ns)
model2.ns <- cv.glmnet(x=dummies[, -1], y=train.y.ns, family="binomial", alpha=1, nfolds=5)

par(mfrow=c(1,2))
plot(model2.ns)
plot(model2.ns$glmnet.fit)

val.ns = cbind(FlagAIB = val.y, val.x.ns)
dummies.val <- model.matrix(FlagAIB~., data=val.ns)
pred2.ns <- predict.cv.glmnet(model2.ns, newx=dummies[, -1],
                           s="lambda.1se", type="response")
(auc2 <- roc(train.y.ns, as.numeric(pred2.ns))$auc)
#AUC_train = 0.88
#AUC_val = 0.8723

# gam
library(splines)
library(mgcv)
model3.1 <- gam(decision~ns(gpa, df=3)+ns(gre,df=3)+
                  ns(uni_faculty, df=3)+ns(uni_pub, df=3)+
                  degree+status+degree:status+
                  decision_method+decision_month,
                data=train, family="binomial")
summary(model3.1)
model3.2 <- gam(decision~lo(gpa, span=0.2)+lo(gre, span=0.2)+
                  lo(uni_faculty, span=0.2)+lo(uni_pub, span=0.2)+
                  degree+status+degree:status+
                  decision_method+decision_month, 
                data=train, family="binomial")
summary(model3.2)
AIC(model3.1, model3.2)
pred3 <- predict.gam(model3.1, newdata=test.x, type="response")
auc3 <- roc(test.y, pred3)$auc
