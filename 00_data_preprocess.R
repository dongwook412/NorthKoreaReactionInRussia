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

data <- fromJSON('data/sentiment_analysis/raw/russian_sentiment_train.json')

### Annotate data

for (i in 24:ceiling(nrow(data) / 100)) {
  if (i == 83) {
    train <- data[8201:8263,]
    train_annotate <-
      udpipe_annotate(ud_model_rs, x = train$text, doc_id = train$id)
    train_annotate <- as_data_frame(train_annotate)
    write_feather(train_annotate,'data/annotate_train/train_annotate_8261-8263.feather')
    cat('finished at {Sys.time()}')
  }
  
  train <-
    data[(100 * (i - 1) + 1):(100 * i),] # 1~100, 101~200 ...
  train_annotate <-
    udpipe_annotate(ud_model_rs, x = train$text, doc_id = train$id)
  train_annotate <- as_data_frame(train_annotate)
  numstr_1 <- sprintf('%04d',(100 * (i - 1) + 1))
  numstr_2 <- sprintf('%04d',(100 * i))
  write_feather(train_annotate, glue('data/annotate_train/train_annotate_{numstr_1}-{numstr_2}.feather'))
  cat(glue('{(100 * (i - 1) + 1)}-{(100 * i)} is finished at {Sys.time()}'))
  cat('\n')
}

## 2301~2400
## i <- 25

### Collect data

files <- paste0("data/annotate_train/",list.files('data/annotate_train',pattern = "*.feather"))

annotate_train <-
  files %>% map_df(read_feather)

write_feather(annotate_train, 'data/sentiment_analysis/pre_processed/annotate_train.feather')

## data/russian_sentiment_dictionary.csv

### Load data

dictionary <- read_csv('data/sentiment_analysis/raw/russian_sentiment_dictionary.csv')

### Annotate data

colnames(dictionary) <- c("sentence", "score")
dictionary_annotate <- udpipe_annotate(ud_model_rs, x = dictionary$sentence)
dictionary_annotate <- as_data_frame(dictionary_annotate)

annotate_dict <- dictionary_annotate %>% left_join(dictionary)
annotate_dict <- unique(annotate_dict)
write_feather(annotate_dict, 'data/sentiment_analysis/pre_processed/annotate_dict.feather')

