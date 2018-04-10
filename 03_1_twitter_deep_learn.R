library(keras)
library(dplyr)
library(jsonlite)
library(readr)
library(magrittr)
library(caTools)
# samples <- c("The cat sat on the mat.", "The dog ate my homework.")

twitter_data <- read_csv('Data/Train/Twitter/dataframe.csv')

set.seed(1993)




twitter_train_ind <- sample.split(twitter_data$tone, SplitRatio = 0.9)

twitter_train <- twitter_data[twitter_train_ind,]
twitter_test <- twitter_data[!twitter_train_ind, ]

max_features <- 500
maxlen <- 200

twitter_train_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(twitter_train$text)

twitter_train_sequences <- texts_to_sequences(twitter_train_tokenizer, twitter_train$text)

twitter_train_x <- pad_sequences(twitter_train_sequences, maxlen)


twitter_model <- keras_model_sequential()%>%
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


twitter_model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

twitter_history <- twitter_model %>% fit(
  twitter_train_x, 
  (as.numeric(as.factor(twitter_train$tone)) - 1),
  epochs = 10,
  batch_size = 128,
  validation_split = 0.1
)


twitter_test_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(twitter_test$text)

twitter_test_sequences <- texts_to_sequences(twitter_test_tokenizer, twitter_test$text)

twitter_test_x <- pad_sequences(twitter_test_sequences, max_len)

mean(ifelse(predict(twitter_model, twitter_test_x) > 0.5, 1, 0) == twitter_test$tone) # 92%
