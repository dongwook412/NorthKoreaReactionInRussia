# 01_3_Sentiment_Dictionary_Expret

library(feather)
library(dplyr)
library(jsonlite)
library(magrittr)

annotate_train <- read_feather('Data/Train/Expert/annotate_train.feather')
annotate_dict <- read_feather('Data/SentimentDictionary/annotate_dict.feather')

score <- annotate_dict %>% select(lemma, score)
annotate_train <- annotate_train %>% left_join(score, by = 'lemma')

predict_score <- annotate_train %>% group_by(doc_id) %>% summarise(score = mean(score, na.rm = TRUE))
predict_score$doc_id <- as.numeric(predict_score$doc_id)

train <- read_csv('Data/Train/Expert/data.csv')
colnames(train) <- c('text', 'sentiment', 'doc_id')

predict_score <- predict_score %>% arrange(doc_id)
train <- train %>% arrange(doc_id)

predict_score <- subset(predict_score,subset = c(rep(TRUE, 6999), FALSE))

train$score <- predict_score$score

# readr::write_csv(train, 'data/sentiment_analysis/analysis/setiment_dict.csv')

train %>% group_by(sentiment) %>% summarise(mean(score, na.rm = TRUE))

train %<>% filter(sentiment %in% c('n','p','l'))

train <- train %>% mutate(predict = case_when(.$score < 0 ~ 'n',
                                              .$score > 1 ~ 'p',
                                              TRUE ~ 'l'))


# train <- train %>% filter(sentiment %in% c('negative', 'positive'))
table(train$sentiment)
table(train$sentiment, train$predict)

# about 54%

