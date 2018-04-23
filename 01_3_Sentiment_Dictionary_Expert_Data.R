# 01_3_Sentiment_Dictionary_Expret

library(feather)
library(dplyr)
library(jsonlite)
library(magrittr)



data <- read_csv('Data/Train/Expert/data.csv')
set.seed(1993)
data <- rbind(data %>% filter(sentiment != "l"), (data %>% filter(sentiment == 'l'))[sample(2917, 550), ])
colnames(data)[3] <- 'doc_id'

score <- read_csv('Result/Dictionary/expert_dict_score.csv')
score$total_score <- rowSums(score[, 2:6])

data <- data %>% left_join(score, by = 'doc_id')

max <- 0



for(i in seq(-5, 5, 0.5)) {
  
  for(j in seq(-5, 5, 0.5)) {
    cor <- 0
    score %<>% mutate(predict = case_when(total_score > i ~ 'p',
                                          total_score > j ~ 'l',
                                          TRUE ~ 'n'))
    for(k in 1:3995) {
      if(score$predict[k] == data$sentiment[k]) {
        cor <- cor + 1
        if(score$predict[k] == 'p') {
          cor <- cor + 2
        }
        if(score$predict[k] == 'n') {
          cor <- cor + 2
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



score %<>% mutate(predict = case_when(total_score > 4 ~ 'p',
                                        total_score > -5 ~ 'l',
                                        TRUE ~ 'n'))



table(score$predict, data$sentiment)

