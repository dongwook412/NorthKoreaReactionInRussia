library(keras)
library(dplyr)
library(jsonlite)
library(readr)
library(magrittr)
library(caTools)

twitter_model <- load_model_hdf5('DeepLearnModel/twitter_model.hdf5') # binary 
kaggle_model <- load_model_hdf5('DeepLearnModel/kaggle_model.hdf5')  # 1 neg 2 neu 3 pos
kaggle_binary_model <- load_model_hdf5('DeepLearnModel/kaggle_binary_model.hdf5')
expert_model <- load_model_hdf5('DeepLearnModel/expert_model.hdf5') # 1 l 2 n 3 p 
expert_binary_model <- load_model_hdf5('DeepLearnModel/expert_binary_model.hdf5')


twitter_data <- read_csv('Data/Train/Twitter/dataframe.csv')
kaggle_data <- fromJSON('Data/Train/Kaggle/russian_sentiment_train.json')
expert_data <- read_csv('Data/Train/Expert/data.csv')
expert_data %<>% filter(sentiment != "л")

max_features <- 500
maxlen <- 200

# from twitter

## to expert

expert_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(expert_data$text)

expert_test_sequences <- texts_to_sequences(expert_tokenizer, expert_data$text)

expert_x <- pad_sequences(expert_test_sequences, maxlen)

pred <- predict(twitter_model, expert_x)


data.frame(pred_num = pred) %>% mutate(pred = case_when(pred_num < 1/3 ~ 'n',
                                                        pred_num < 2/3 ~ 'l',
                                                        TRUE ~ 'p')) -> pred
# 1 neg 2 neu 3 pos


mean(pred$pred == expert_data$sentiment)

table(pred$pred, expert_data$sentiment)


## to kaggle

kaggle_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(kaggle_data$text)

kaggle_test_sequences <- texts_to_sequences(kaggle_tokenizer, kaggle_data$text)

kaggle_x <- pad_sequences(kaggle_test_sequences, maxlen)

pred <- predict(twitter_model, kaggle_x)


data.frame(pred_num = pred) %>% mutate(pred = case_when(pred_num < 1/3 ~ 'negative',
                                                        pred_num < 2/3 ~ 'neutral',
                                                        TRUE ~ 'positive')) -> pred
# 1 l 2 n 3 p


mean(pred$pred == kaggle_data$sentiment)

table(pred$pred, kaggle_data$sentiment)


# from kaggle

## to twitter
set.seed(1)
twitter_data <- twitter_data[sample.split(twitter_data$sentiment, 0.1), ]
twitter_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(twitter_data$text)

twitter_test_sequences <- texts_to_sequences(twitter_tokenizer, twitter_data$text)

twitter_x <- pad_sequences(twitter_test_sequences, maxlen)

pred <- predict(expert_binary_model, twitter_x)



mean(ifelse(pred > 0.5, 'positive', 'negative') == twitter_data$sentiment)


## to expert

expert_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(expert_data$text)

expert_test_sequences <- texts_to_sequences(expert_tokenizer, expert_data$text)

expert_x <- pad_sequences(expert_test_sequences, maxlen)

pred <- predict(kaggle_model, expert_x)

# 1 neg 2 neu 3 pos
expert_data %>% mutate(for_test = case_when(sentiment == 'n' ~ 1,
                          sentiment == 'l' ~ 2,
                          sentiment == 'p' ~ 3)) %>% select(for_test) -> for_test


mean(max.col(pred, ties.method = c("random")) == for_test)



# from expert

## to twitter

set.seed(1)
twitter_data <- twitter_data[sample.split(twitter_data$sentiment, 0.1), ]
twitter_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(twitter_data$text)

twitter_test_sequences <- texts_to_sequences(twitter_tokenizer, twitter_data$text)

twitter_x <- pad_sequences(twitter_test_sequences, maxlen)

pred <- predict(expert_binary_model, twitter_x)



mean(ifelse(pred > 0.5, 'positive', 'negative') == twitter_data$sentiment)

## to kaggle

kaggle_tokenizer <- text_tokenizer(num_words = max_features) %>%
  fit_text_tokenizer(kaggle_data$text)

kaggle_test_sequences <- texts_to_sequences(kaggle_tokenizer, kaggle_data$text)

kaggle_x <- pad_sequences(kaggle_test_sequences, maxlen)

pred <- predict(expert_model, kaggle_x)

# 1 l 2 n 3 p 
kaggle_data %>% mutate(for_test = case_when(sentiment == 'neutral' ~ 1,
                                            sentiment == 'negative' ~ 2,
                                            sentiment == 'positive' ~ 3)) %>% select(for_test) -> for_test


mean(max.col(pred, ties.method = c("random")) == for_test)

