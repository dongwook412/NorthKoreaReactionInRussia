# precision
library(tidyverse)
library(magrittr)

precision <- read_csv("~/Desktop/precision.csv")

table(precision$sentiment, precision$result)

test_pos <- precision %>% filter(result == "p")
test_neg <- precision %>% filter(result == "n")

precision_pos <- sum(test_pos$sentiment == "p")/nrow(test_pos) # 73%
precision_neg <- sum(test_neg$sentiment == "n")/nrow(test_neg) # 72%


test_pos %<>% filter(proba > 0.7)
test_neg %<>% filter(proba > 0.7)

precision_pos <- sum(test_pos$sentiment == "p")/nrow(test_pos) # 94%
precision_neg <- sum(test_neg$sentiment == "n")/nrow(test_neg) # 90%


test_pos %<>% filter(proba > 0.8)
test_neg %<>% filter(proba > 0.8)

precision_pos <- sum(test_pos$sentiment == "p")/nrow(test_pos) # 93%
precision_neg <- sum(test_neg$sentiment == "n")/nrow(test_neg) # 88%
