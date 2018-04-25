model <- keras_model_sequential()

model %>%
  layer_dense(units=32, activation="relu",
              input_shape=c(ncol(train.x.bin)),
              kernel_initializer='uniform') %>%
  layer_batch_normalization() %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units=64, activation="tanh",
              kernel_initializer='he_normal') %>%
  layer_batch_normalization() %>%
  layer_dropout(rate=0.2) %>%
  # layer_dense(units=128, activation="relu",
  #             kernel_initializer='uniform') %>%
  # layer_batch_normalization() %>%
  # layer_dropout(rate=0.4) %>%
  # layer_dense(units=64, activation="relu",
  #             kernel_initializer='he_normal') %>%
  # layer_batch_normalization() %>%
  # layer_dropout(rate=0.3) %>%
  # layer_dense(units=16, activation="tanh",
  #             kernel_initializer='uniform') %>%
  # layer_batch_normalization() %>%
  # layer_dropout(rate=0.1) %>%
  layer_dense(units=2, activation="softmax")

model %>% compile(
  loss="categorical_crossentropy",
  optimizer=optimizer_adam(lr=0.0015),
  metrics=c("accuracy")
)
train.y.bin <- to_categorical(train.y$FlagAIB, 2)

model %>% fit(
  x=as.matrix(train.x.bin), y=train.y.bin,
  epochs=150, batch_size=128,
  validation_split=0.1
)

test.y <- model %>% predict_proba(as.matrix(test.x.bin))