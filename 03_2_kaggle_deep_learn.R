library(keras)
library(dplyr)
library(jsonlite)
library(readr)
library(magrittr)
library(caTools)
# samples <- c("The cat sat on the mat.", "The dog ate my homework.")

kaggle_data <- fromJSON('Data/Train/Kaggle/russian_sentiment_train.json')

set.seed(1993)

kaggle_train_ind <- sample.split(kaggle_data$sentiment, SplitRatio = 0.5)

kaggle_train <- kaggle_data[kaggle_train_ind,]
kaggle_test <- kaggle_data[!kaggle_train_ind,]

max_features <- 500
maxlen <- 200

kaggle_train_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(kaggle_train$text)

kaggle_train_sequences <- texts_to_sequences(kaggle_train_tokenizer, kaggle_train$text)

kaggle_train_x <- pad_sequences(kaggle_train_sequences, maxlen)


kaggle_model <- keras_model_sequential()%>%
  # Creates dense embedding layer; outputs 3D tensor
  # with shape (batch_size, sequence_length, output_dim)
  layer_embedding(input_dim = max_features, 
                  output_dim = 128,
                  input_length = maxlen
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
  bidirectional(layer_cudnn_gru(units = 64)) %>% 
  layer_dropout(rate = 0.7) %>% 
  layer_dense(units = 16, activation = 'tanh') %>% 
  layer_dropout(rate = 0.7) %>% 
  layer_dense(units = 4, activation = 'tanh') %>% 
  layer_dropout(rate = 0.7) %>% 
  layer_dense(units = 3, activation = 'softmax')


kaggle_model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

kaggle_history <- kaggle_model %>% fit(
  kaggle_train_x, 
  to_categorical(as.numeric(as.factor(kaggle_train$sentiment)) - 1),
  epochs = 100,
  batch_size = 128,
  validation_split = 0.1
)


kaggle_test_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(kaggle_test$text)

kaggle_test_sequences <- texts_to_sequences(kaggle_test_tokenizer, kaggle_test$text)

kaggle_test_x <- pad_sequences(kaggle_test_sequences, maxlen)

pred <- predict(kaggle_model, kaggle_test_x)

# 1 neg 2 neu 3 pos


mean(max.col(pred, ties.method = c("random")) == as.numeric(as.factor(kaggle_test$sentiment)))

keras::save_model_hdf5(kaggle_model, 'DeepLearnModel/kaggle_model.hdf5')
