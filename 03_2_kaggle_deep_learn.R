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
  layer_flatten() %>% 
  layer_dense(units = 64, activation = 'tanh') %>%
  layer_dropout(rate = 0.7) %>% 
  layer_dense(units = 16, activation = 'tanh') %>% 
  layer_dropout(rate = 0.7) %>% 
  layer_dense(units = 4, activation = 'tanh') %>% 
  layer_dropout(rate = 0.7) %>% 
  layer_dense(units = 1, activation = 'sigmoid')


kaggle_model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

kaggle_history <- kaggle_model %>% fit(
  kaggle_train_x, 
  (as.numeric(as.factor(kaggle_train$sentiment)) - 1),
  epochs = 10,
  batch_size = 128,
  validation_split = 0.1
)


kaggle_test_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(kaggle_test$text)

kaggle_test_sequences <- texts_to_sequences(kaggle_test_tokenizer, kaggle_test$text)

kaggle_test_x <- pad_sequences(kaggle_test_sequences, max_len)

mean(ifelse(predict(kaggle_model, kaggle_test_x) > 0.5, 1, 0) == kaggle_test$sentiment)
