devtools::install_github('jjwkdl/AzureTA')
library(AzureTA)
library(magrittr)

az <- AzureTA(API_KEY = '', LOCATION = 'westcentralus')

az %>% get_sentiment()
az %>% get_key_phrases()
