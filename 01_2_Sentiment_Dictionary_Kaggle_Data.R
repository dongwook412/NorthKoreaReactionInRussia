# 03_1_Sentiment_Dictionary_Spoken_Data


library(feather)
library(dplyr)
library(jsonlite)
library(magrittr)
annotate_train <- read_feather('Data/Train/Kaggle/annotate_train.feather')
# 
# 
kaggle_dict <- read_csv('Data/SentimentDictionary/Kaggle/lemma_dict.csv')
bing_dict <- read_csv('Data/SentimentDictionary/EngLexicon/rus_bing.csv')
afinn_dict <- read_csv('Data/SentimentDictionary/EngLexicon/rus_AFINN.csv')
nrc_dict <- read_csv('Data/SentimentDictionary/EngLexicon/rus_nrc.csv')
loghran_dict <- read_csv('Data/SentimentDictionary/EngLexicon/rus_loghran.csv')
# 
# 
kaggle_score <- annotate_train %>% left_join(kaggle_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(kaggle_score = mean(score, na.rm = TRUE))
bing_score <- annotate_train %>% left_join(bing_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(bing_score = mean(score, na.rm = TRUE))
afinn_score <- annotate_train %>% left_join(afinn_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(afinn_score = mean(score, na.rm = TRUE))
nrc_score <- annotate_train %>% left_join(nrc_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(nrc_score = mean(score, na.rm = TRUE))
loghran_score <- annotate_train %>% left_join(loghran_dict, by = "lemma") %>% group_by(doc_id) %>% summarise(loghran_score = mean(score, na.rm = TRUE))

write_csv(kaggle_score, 'kaggle_score.csv')


data <- read_csv('Data/Train/Kaggle/sample.csv')

score <- read_csv('Result/Dictionary/kaggle_dict_score.csv')
data$doc_id <- 1:nrow(data)
score$total_score <- rowSums(score[, 2:6])
score <- score[1:826, ]

max <- 0

for(i in seq(-5, 5, 0.5)) {
  for(j in seq(-5, 5, 0.5)) {
    cor <- 0
    score %<>% mutate(predict = case_when(total_score > i ~ 'positive',
                                          total_score > j ~ 'neutral',
                                          TRUE ~ 'negative'))
  
    for(k in 1:826) {
      if(score$predict[k] == data$sentiment[k]) {
        cor <- cor + 1
        if(score$predict[k] == 'positive') {
          cor <- cor + 0.4
        }
        if(score$predict[k] == 'negative') {
          cor <- cor + 1.2
        }
      } 
    }
     
    if (max < cor) {
      max <- cor
      obj_i <- i
      obj_j <- j
      cat(paste("max : ", max, "\n", obj_i, obj_j, "\n"))
    }
    
  }
}

score %<>% mutate(predict = case_when(total_score > 2 ~ 'positive',
                                      total_score > -4.5 ~ 'neutral',
                                      TRUE ~ 'negative'))

table(score$predict, data$sentiment)
