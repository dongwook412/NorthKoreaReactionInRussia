library(keras)
library(dplyr)
library(jsonlite)
library(readr)
library(magrittr)
library(caTools)
# samples <- c("The cat sat on the mat.", "The dog ate my homework.")

expert_data <- read_csv('Data/Train/Expert/data.csv')
expert_data %<>% filter(sentiment != "л")
set.seed(1993)




expert_train_ind <- sample.split(expert_data$sentiment, SplitRatio = 0.9)

expert_train <- expert_data[expert_train_ind,]
expert_test <- expert_data[!expert_train_ind, ]

max_features <- 500
maxlen <- 200

expert_train_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(expert_train$text)

expert_train_sequences <- texts_to_sequences(expert_train_tokenizer, expert_train$text)

expert_train_x <- pad_sequences(expert_train_sequences, maxlen)


expert_model <- keras_model_sequential()%>%
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


expert_model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

expert_history <- expert_model %>% fit(
  expert_train_x, 
  to_categorical(as.numeric(as.factor(expert_train$sentiment)) - 1),
  epochs = 100,
  batch_size = 128,
  validation_split = 0.1
)

expert_test_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(expert_test$text)

expert_test_sequences <- texts_to_sequences(expert_test_tokenizer, expert_test$text)

expert_test_x <- pad_sequences(expert_test_sequences, maxlen)

pred <- predict(expert_model, expert_test_x)
# 1 l 2 n 3 p 
mean(max.col(pred, ties.method = c("random")) == as.numeric(as.factor(expert_test$sentiment)))

keras::save_model_hdf5(expert_model, 'DeepLearnModel/expert_model.hdf5')
