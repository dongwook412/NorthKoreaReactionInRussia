# 01_1_Sentiment_Dictionary_Spoken_Data


library(feather)
library(dplyr)
library(jsonlite)

annotate_train <- read_feather('Data/Train/Twitter/annotate_train.feather')
annotate_dict <- read_feather('Data/SentimentDictionary/annotate_dict.feather')

score <- annotate_dict %>% select(lemma, score)
annotate_train <- annotate_train %>% left_join(score, by = 'lemma')

predict_score <- annotate_train %>% group_by(doc_id) %>% summarise(score = mean(score, na.rm = TRUE))
predict_score$doc_id <- as.numeric(predict_score$doc_id)

train <- readr::read_csv('Data/SpokenData/dataframe.csv')
train$doc_id <- 1:nrow(train)
colnames(train) <- c('text', 'sentiment', 'doc_id')

predict_score <- predict_score %>% arrange(doc_id)
train <- train %>% arrange(doc_id)
train <- train[1:106500, ]
train$score <- predict_score$score

# readr::write_csv(train, 'data/sentiment_analysis/analysis/setiment_dict.csv')

train %>% group_by(sentiment) %>% summarise(mean(score, na.rm = TRUE))

train <- train %>% filter(!is.nan(score))
train <- train %>% mutate(predict = case_when(.$score < 0 ~ 'negative',
                                              TRUE ~ 'positive'))


# train <- train %>% filter(sentiment %in% c('negative', 'positive'))
table(train$sentiment)
table(train$sentiment, train$predict)

# about 60%

