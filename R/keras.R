if(!require(keras)) install.packages("keras")
library(keras)
model <- keras_model_sequential()
model %>%
  layer_dense(units=32, activation="relu",
              input_shape=c(ncol(train.x)),
              kernel_initializer='uniform') %>%
  layer_batch_normalization() %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units=64, activation="tanh",
              kernel_initializer='he_normal') %>%
  layer_batch_normalization() %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units=128, activation="relu",
               kernel_initializer='uniform') %>%
   layer_batch_normalization() %>%
   layer_dropout(rate=0.2) %>%
  layer_dense(units=64, activation="relu",
              kernel_initializer='he_normal') %>%
  layer_batch_normalization() %>%
  layer_dropout(rate=0.2) %>%
   layer_dense(units=32, activation="relu",
               kernel_initializer='he_normal') %>%
   layer_batch_normalization() %>%
   layer_dropout(rate=0.2) %>%
   layer_dense(units=16, activation="tanh",
               kernel_initializer='uniform') %>%
   layer_batch_normalization() %>%
   layer_dropout(rate=0.1) %>%
  layer_dense(units=2, activation="softmax")

model %>% compile(
  loss="categorical_crossentropy",
  optimizer=optimizer_adam(lr=0.0015),
  metrics=c("accuracy")
)

train.y.bin <- to_categorical(train.y, 2)

model %>% fit(
  x=as.matrix(train.x), y=train.y.bin,
  epochs=90, batch_size=128,
  validation_split=0.1
)


pred.y = predict_proba(model,as.matrix(val.x))
pred = prediction(pred.y[,2], val.y)
pred_AUC = performance(pred, "auc")
(AUC.model = pred_AUC@y.values[[1]])

####AUC########LAYER#############RATE
#0.8673957
#0.8673734
#0.8691925      4            
#0.8669083      4   
#0.8692052      5
test.y <- model %>% predict_proba(as.matrix(test.x.bin))