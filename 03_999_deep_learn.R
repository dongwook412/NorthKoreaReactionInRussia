library(keras)

data_x <- readRDS('2_DataPreprocess/DeepLearn/data_x.rds')
data_y <- readRDS('2_DataPreprocess/DeepLearn/data_y.rds')
tfidf_df <- feather::read_feather('2_DataPreprocess/DeepLearn/tfidf_df.feather')

data_y <- as.factor(data_y)
data_y <- to_categorical(data_y)
data_y <- data_y[,2:4]


max_features <- nrow(tfidf_df)
maxlen <- 500

input_train <- pad_sequences(data_x, maxlen = maxlen)



model <- keras_model_sequential()%>%
  # Creates dense embedding layer; outputs 3D tensor
  # with shape (batch_size, sequence_length, output_dim)
  layer_embedding(input_dim = max_features, 
                  output_dim = 128,
                  input_length = maxlen,
                  embeddings_initializer = initializer_glorot_uniform()
                  ) %>% 
  layer_conv_1d(
    filters = 64,
    kernel_size = 5,
    padding = 'valid',
    activation = 'relu',
    strides = 1
  ) %>% 
  layer_max_pooling_1d(pool_size = 4) %>% 
  layer_dropout(rate = 0.7) %>% 
  layer_conv_1d(
    filters = 64,
    kernel_size = 5,
    padding = 'valid',
    activation = 'relu',
    strides = 1
  ) %>% 
  layer_max_pooling_1d(pool_size = 4) %>% 
  layer_dropout(rate = 0.7) %>% 
  bidirectional(layer_cudnn_gru(units = 64)) %>%
  layer_dropout(rate = 0.7) %>% 
  layer_dense(units = 32, activation = 'relu', kernel_regularizer = regularizer_l2(0.001)) %>% 
  layer_dropout(rate = 0.7) %>% 
  layer_dense(units = 16, activation = 'relu', kernel_regularizer = regularizer_l2(0.001)) %>% 
  layer_dense(units = 3, activation = 'softmax')


model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  input_train, data_y,
  epochs = 100,
  batch_size = 512,
  validation_split = 0.1
)
