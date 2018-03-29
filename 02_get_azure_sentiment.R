library(httr)
library(jsonlite)
library(dplyr)
library(caTools)
## Get Sentiment function..

get_azure_sentiment <- function(df, key) {
  url <- 'https://westcentralus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment'
  headers <- c('Ocp-Apim-Subscription-Key' = key,
               'Content-Type' = 'application/json',
               'Accept' = 'application/json')
  to_post <- list(documents = df)
  res <- POST(url, add_headers(.headers = headers), body = to_post, encode = 'json')
  fromJSON(content(res, as = 'text'))$documents
}


df <- readr::read_csv('2_DataPreprocess/AzureAPI/data.csv')
df$language = 'ru'
df$id = 1:nrow(df)

set.seed(1993)
ind <- caTools::sample.split(df$tone, SplitRatio = 0.003)
test <- df[ind,]
input <- test %>% select(id, language, text)
key <- '843de0dcd474479183d24ce881d830ad'


h <- get_azure_sentiment(input, key)
h <- h %>% mutate(pred = case_when(score > 0.5 ~ 'positive',
                              TRUE ~ 'negative'))


table(test$tone, h$pred)
