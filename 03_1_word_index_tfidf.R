library(udpipe)
library(feather)
library(stringr)
library(dplyr)
library(purrr)
train <- read_feather('2_DataPreprocess/DeepLearn/annotate_train.feather')

lemma <- train$lemma
lemma <- str_to_lower(train$lemma)
lemma <- str_replace_all(lemma, '[^[:alpha:]=\\.]', '')
lemma <- str_replace_all(lemma, '[.]','')

train$lemma <- lemma


str(train)

train$doc_id <- as.numeric(train$doc_id)
train$token_id <- as.numeric(train$token_id)
train$head_token_id <- as.numeric(train$head_token_id)


# write_feather(train, '2_DataPreprocess/DeepLearn/trim_lemma_train.feather')


dtf <- document_term_frequencies(train[, c("doc_id", "lemma")])
tf <- dtf %>% group_by(term) %>% summarise(freq = sum(freq)) %>% filter(freq > 10)
dtf <- dtf %>% left_join(tf, by = )
dtm <- document_term_matrix(dtf)
tfidf <- dtm_tfidf(dtm)

tfidf_df <- data_frame(lemma = names(tfidf), score = tfidf)
tfidf_df <- tfidf_df %>% filter(lemma %in% tf$term)
tfidf_df <- tfidf_df %>% filter(score > 0.01)
tfidf_df <- tfidf_df %>% mutate(lemma_id = 1:nrow(tfidf_df))


word_index <- train %>% left_join(tfidf_df, by = "lemma")
# write_feather(tfidf_df, '2_DataPreprocess/DeepLearn/tfidf_df.feather')
# write_feather(word_index, '2_DataPreprocess/DeepLearn/word_index.feather')

list_array <- function(array, na.rm = FALSE){
  if(na.rm){
    ret <- na.omit(array)
    return(list(ret))
  }
  list(array)
}

# list_array(c(1, 2, NA, 5), na.rm = TRUE)


index_string <- word_index %>% group_by(doc_id) %>% summarise(string = list_array(lemma_id, na.rm = TRUE)) %>% `$`(string)

saveRDS(index_string, '2_DataPreprocess/DeepLearn/data_x.rds')
