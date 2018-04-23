# 01_1_Sentiment_Dictionary_Spoken_Data


# library(feather)
# library(dplyr)
# library(jsonlite)
# library(magrittr)
# annotate_train <- read_feather('Data/Train/Twitter/annotate_train.feather')
# 
# 
# kaggle_dict <- read_csv('Data/SentimentDictionary/Kaggle/lemma_dict.csv')
# bing_dict <- read_csv('Data/SentimentDictionary/EngLexicon/rus_bing.csv')
# afinn_dict <- read_csv('Data/SentimentDictionary/EngLexicon/rus_AFINN.csv')
# nrc_dict <- read_csv('Data/SentimentDictionary/EngLexicon/rus_nrc.csv')
# loghran_dict <- read_csv('Data/SentimentDictionary/EngLexicon/rus_loghran.csv')
# 
# 
# kaggle_score <- annotate_train %>% left_join(kaggle_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(kaggle_score = mean(score, na.rm = TRUE))
# bing_score <- annotate_train %>% left_join(bing_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(bing_score = mean(score, na.rm = TRUE))
# afinn_score <- annotate_train %>% left_join(afinn_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(afinn_score = mean(score, na.rm = TRUE))
# nrc_score <- annotate_train %>% left_join(nrc_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(nrc_score = mean(score, na.rm = TRUE))
# loghran_score <- annotate_train %>% left_join(loghran_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(loghran_score = mean(score, na.rm = TRUE))
# 
# write_csv(loghran_score, 'loghran_score.csv')


data <- read_csv('Data/Train/Twitter/sample.csv')

score <- read_csv('Result/Dictionary/twitter_dict_score.csv')
data$doc_id <- 1:nrow(data)
score$total_score <- rowSums(score[, 2:6])
score <- score[1:13802, ]


score %<>% mutate(predict = case_when(total_score > -1 ~ 'positive',
                                      TRUE ~ 'negative'))



table(score$predict, data$sentiment)
