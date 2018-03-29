# Data preprocess

library(dplyr)
library(purrr)
library(jsonlite)
library(readr)
library(feather)
library(glue)
library(udpipe)

## Load Model

ud_model_rs <- udpipe_download_model(language = 'russian-syntagrus')
ud_model_rs <- udpipe_load_model(ud_model_rs$file_model)

## russian_sentiment_train.json

### Load data

data <- fromJSON('1_DataCrawl/DataMining/russian_sentiment_train.json')

### Annotate data

for (i in 24:ceiling(nrow(data) / 100)) {
  if (i == 83) {
    train <- data[8201:8263,]
    train_annotate <-
      udpipe_annotate(ud_model_rs, x = train$text, doc_id = train$id)
    train_annotate <- as_data_frame(train_annotate)
    write_feather(train_annotate,'temp/train_annotate_8261-8263.feather')
    cat('finished at {Sys.time()}')
  }
  
  train <-
    data[(100 * (i - 1) + 1):(100 * i),] # 1~100, 101~200 ...
  train_annotate <-
    udpipe_annotate(ud_model_rs, x = train$text, doc_id = train$id)
  train_annotate <- as_data_frame(train_annotate)
  numstr_1 <- sprintf('%04d',(100 * (i - 1) + 1))
  numstr_2 <- sprintf('%04d',(100 * i))
  write_feather(train_annotate, glue('temp/train_annotate_{numstr_1}-{numstr_2}.feather'))
  cat(glue('{(100 * (i - 1) + 1)}-{(100 * i)} is finished at {Sys.time()}'))
  cat('\n')
}

## 2301~2400
## i <- 25

### Collect data

files <- paste0("temp/",list.files('temp',pattern = "*.feather"))

annotate_train <-
  files %>% map_df(read_feather)

write_feather(annotate_train, 'data/sentiment_analysis/pre_processed/annotate_train.feather')
write_feather(annotate_train, '2_DataPreprocess/DeepLearn/annotate_train.feather')

## data/russian_sentiment_dictionary.csv

### Load data

dictionary <- read_csv('2_DataPreprocess/SentimentDictionary/russian_sentiment_dictionary.csv')

### Annotate data

colnames(dictionary) <- c("sentence", "score")
dictionary_annotate <- udpipe_annotate(ud_model_rs, x = dictionary$sentence)
dictionary_annotate <- as_data_frame(dictionary_annotate)

annotate_dict <- dictionary_annotate %>% left_join(dictionary)
annotate_dict <- unique(annotate_dict)
write_feather(annotate_dict, '2_DataPreprocess/SentimentDictionary/annotate_dict.feather')

# Using Sentiment Dictionary

## <- TDF TDM TDM TF-IDF ->

library(feather)
library(dplyr)
library(jsonlite)

annotate_train <- read_feather('data/pre_processed/annotate_train.feather')
annotate_dict <- read_feather('data/pre_processed/annotate_dict.feather')

score <- annotate_dict %>% select(lemma, score)
annotate_train <- annotate_train %>% left_join(score, by = 'lemma')

predict_score <- annotate_train %>% group_by(doc_id) %>% summarise(score = mean(score, na.rm = TRUE))
predict_score$doc_id <- as.numeric(predict_score$doc_id)

train <- fromJSON('data/raw/russian_sentiment_train.json')
colnames(train) <- c('text', 'doc_id', 'sentiment')

predict_score <- predict_score %>% arrange(doc_id)
train <- train %>% arrange(doc_id)

train$score <- predict_score$score

# readr::write_csv(train, 'data/sentiment_analysis/analysis/setiment_dict.csv')


train <- train %>% mutate(predict = case_when(.$score < 0 ~ 'negative',
                                              TRUE ~ 'positive'))


train <- train %>% filter(sentiment %in% c('negative', 'positive'))
table(train$sentiment)
table(train$sentiment, train$predict)

# about 68%

