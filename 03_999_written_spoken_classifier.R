library(keras)
library(dplyr)
library(jsonlite)
library(readr)
library(magrittr)
library(caTools)
# samples <- c("The cat sat on the mat.", "The dog ate my homework.")

spoken_data <- read_csv('Data/SpokenData/dataframe.csv')
written_data <- fromJSON('Data/WrittenData/russian_sentiment_train.json')

set.seed(1993)
spoken_data <- written_data[sample(nrow(written_data), 8000), ]

text_data <- c(spoken_data$text, written_data$text)
y_data <- c(rep(0, nrow(spoken_data)), rep(1, nrow(written_data)))

data <- data.frame(text = text_data, class = y_data, stringsAsFactors = FALSE)
data <- data[sample(nrow(data)), ]

train_ind <- sample.split(data$class, SplitRatio = 0.9)

train <- data[train_ind,]

test <- data[!train_ind, ]

max_features <- 200
max_len <- 100

tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(train$text)

sequences <- texts_to_sequences(tokenizer, train$text)

train_x <- pad_sequences(sequences, max_len)


model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 8,
                  input_length = max_len) %>%
  layer_flatten() %>%
  layer_dense(units = 1, activation = "sigmoid")


model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  train_x, 
  train$class,
  epochs = 10,
  batch_size = 512,
  validation_split = 0.1
)


tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(test$text)

sequences <- texts_to_sequences(tokenizer, test$text)

test_x <- pad_sequences(sequences, max_len)
# 0 written_data 1 spoken_data
mean(ifelse(predict(model, test_x) > 0.5, 1, 0) == test$class) # 92%
