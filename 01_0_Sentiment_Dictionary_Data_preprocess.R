# 01_0_Sentiment_Dictionary_Data_preprocess

# Load library

library(dplyr)
library(purrr)
library(jsonlite)
library(readr)
library(feather)
library(glue)
library(udpipe)

# Load Model

ud_model_rs <- udpipe_download_model(language = 'russian-syntagrus')
ud_model_rs <- udpipe_load_model(ud_model_rs$file_model)


## Kaggle Data --------

## russian_sentiment_train.json

### Load data

data <- fromJSON('Data/Train/Kaggle/russian_sentiment_train.json')

### Annotate data

for (i in 1:ceiling(nrow(data) / 100)) {
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

write_feather(annotate_train, 'Data/Train/Kaggle/annotate_train.feather')


## Twitter Data -----------

data <- read_csv('Data/Train/Twitter/dataframe.csv')
data$id <- 1:nrow(data)

for (i in 1700:ceiling(nrow(data) / 100)) {
  if (i == 2761) {
    train <- data[2760001:276048,]
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

files <- paste0("temp/",list.files('temp',pattern = "*.feather"))

annotate_train <-
  files %>% map_df(read_feather)

write_feather(annotate_train, 'Data/Train/Twitter/annotate_train.feather')


## Expert Data ------


data <- read_csv('Data/Train/Expert/data.csv')

for (i in 1:ceiling(nrow(data) / 100)) {
  if (i == 70) {
    train <- data[6901:6999,]
    train_annotate <-
      udpipe_annotate(ud_model_rs, x = train$text, doc_id = train$id)
    train_annotate <- as_data_frame(train_annotate)
    write_feather(train_annotate,'temp/train_annotate_6901-6999.feather')
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

files <- paste0("temp/",list.files('temp',pattern = "*.feather"))

annotate_train <-
  files %>% map_df(read_feather)

write_feather(annotate_train, 'Data/Train/Expert/annotate_train.feather')


## Dictionary Annotate --------

### Load data

dictionary <- read_csv('Data/SentimentDictionary/russian_sentiment_dictionary.csv')

### Annotate data

colnames(dictionary) <- c("sentence", "score")
dictionary_annotate <- udpipe_annotate(ud_model_rs, x = dictionary$sentence)
dictionary_annotate <- as_data_frame(dictionary_annotate)

annotate_dict <- dictionary_annotate %>% left_join(dictionary)
annotate_dict <- unique(annotate_dict)
write_feather(annotate_dict, 'Data/SentimentDictionary/annotate_dict.feather')











