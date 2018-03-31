library(jsonlite)
library(tidyverse)

compare <- fromJSON('3_DataAnalyze/Press/GKG/compare_raw.json')
compare <- compare$tonechart
compare <- rbind(compare %>% filter(bin < -5), compare %>% filter(bin > 5))
compare <- compare %>% filter(count > 0)

df <- data.frame()

for(i in 1:17){
url <- compare$toparts[i][[1]]$url[1]
title <- compare$toparts[i][[1]]$title[1]
score <- compare$bin[i]
df[i, 1] <- url
df[i, 2] <- title
df[i, 3] <- score

}
colnames(df) <- c('url', 'title', 'gdelt')

write_csv(df, '3_DataAnalyze/Press/GKG/compare.csv')
