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
table(expert_data$sentiment)
rbind(
(expert_data %>% filter(sentiment == "n"))[sample(529, 500), ],
(expert_data %>% filter(sentiment == "l"))[sample(2917, 1000), ],
(expert_data %>% filter(sentiment == "p"))[sample(549, 500), ]) -> expert_proc

expert_train_ind <- sample.split(expert_proc$sentiment, SplitRatio = 0.9)

expert_train <- expert_proc[expert_train_ind,]
expert_test <- expert_proc[!expert_train_ind, ]
# expert_train <- expert_proc
max_features <- 500 # extract top 500 words
maxlen <- 200 # 200 words length

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

  bidirectional(layer_cudnn_gru(units = 64, )) %>% 

  layer_dense(units = 16, activation = 'tanh') %>% 

  layer_dense(units = 4, activation = 'tanh') %>% 

  layer_dense(units = 3, activation = 'softmax')


expert_model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

expert_history <- 
  
  
  expert_model %>% fit(
  expert_train_x, 
  to_categorical(as.numeric(as.factor(expert_train$sentiment)) - 1),
  epochs = 10000,
  batch_size = 256,
  validation_split = 0.5,
 verbose = TRUE, view_metrics = FALSE
  )

expert_test_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(expert_test$text)

expert_test_sequences <- texts_to_sequences(expert_test_tokenizer, expert_test$text)

expert_test_x <- pad_sequences(expert_test_sequences, maxlen)

pred <- predict(expert_model, expert_test_x)
# 1 l 2 n 3 p 
mean(max.col(pred, ties.method = c("random")) == as.numeric(as.factor(expert_test$sentiment)))

keras::save_model_hdf5(expert_model, 'DeepLearnModel/expert_model_2.hdf5')
