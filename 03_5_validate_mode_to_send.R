# Organise to send

## Load Data

expert_100 <- read_excel("Data/Organise_to_send/expert_100.xlsx")
kaggle_100 <- read_excel("Data/Organise_to_send/kaggle_100.xlsx")
twitter_100 <- read_excel("Data/Organise_to_send/twitter_100.xlsx")

## Load Model

library(keras)

em <- load_model_hdf5('DeepLearnModel/expert_model.hdf5')
ebm <- load_model_hdf5('DeepLearnModel/expert_binary_model.hdf5')
km <- load_model_hdf5('DeepLearnModel/kaggle_model.hdf5')
kbm <- load_model_hdf5('DeepLearnModel/kaggle_binary_model.hdf5')
tm <-  load_model_hdf5('DeepLearnModel/twitter_model.hdf5')

max_features <- 500
maxlen <- 200

expert_100_tokenizer <- text_tokenizer(num_words = max_features) %>% 
  fit_text_tokenizer(expert_100$text)

expert_100_sequences <- texts_to_sequences(expert_100_tokenizer, expert_100$text)
expert_100_x <- pad_sequences(expert_100_sequences, maxlen)

pred <- predict(em, expert_100_x)

expert_100 %>% mutate(for_test = case_when(sentiment == 'l' ~ 1,
                                            sentiment == 'n' ~ 2,
                                            sentiment == 'p' ~ 3)) %>% select(for_test) -> for_test


mean(max.col(pred, ties.method = c("random")) == for_test$for_test, na.rm = TRUE)

expert_100$`DL(Expert)` <- c(max.col(pred, ties.method = c("random")))


max.col(predict(km, expert_100_x)) # 1 neg 2 neu 3 pos

data.frame(pred_num = predict(tm, expert_100_x)) %>% mutate(pred = case_when(pred_num < 1/3 ~ 'negative',
                                                                         pred_num < 2/3 ~ 'neutral',
                                                                         TRUE ~ 'positive'))





kaggle_100_tokenizer <- text_tokenizer(num_words = max_features) %>% 
  fit_text_tokenizer(kaggle_100$text)

kaggle_100_sequences <- texts_to_sequences(kaggle_100_tokenizer, kaggle_100$text)
kaggle_100_x <- pad_sequences(kaggle_100_sequences, maxlen)

predict(em, kaggle_100_x)
predict(km, kaggle_100_x)
predict(tm, kaggle_100_x)

twitter_100_tokenizer <- text_tokenizer(num_words = max_features) %>% 
  fit_text_tokenizer(twitter_100$text)

twitter_100_sequences <- texts_to_sequences(twitter_100_tokenizer, twitter_100$text)
twitter_100_x <- pad_sequences(twitter_100_sequences, maxlen)

predict(ebm, kaggle_100_x)
predict(kbm, kaggle_100_x)
predict(tm, kaggle_100_x)
