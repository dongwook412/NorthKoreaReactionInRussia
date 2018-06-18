library(tidyverse)
library(magrittr)
library(viridis)
aif_article <- paste0('aif_article/',list.files('aif_article/'))
aif_reply<- paste0('aif_reply/',list.files('aif_reply/'))
echo_article <- paste0('echo_article/',list.files('echo_article/'))
echo_reply <- paste0('echo_reply/',list.files('echo_reply/'))

# readLines(aif_article[1])
# 2 ~ neutral(l) 1 ~ negative(n) 0 ~ positive(p)

read_my_data <- function(vec) {
  num <- readLines(vec)
  date <- vec
  date %<>% str_remove(pattern = ".txt")
  date %<>%str_remove(pattern = regex("\\w*\\/"))
  date %<>% paste0("-01")
  
  date 
  df <- data_frame(date = date, predict = num)
  df %<>% mutate(sentiment = case_when(predict == 0 ~ 1,
                                       predict == 1 ~ -1,
                                       predict == 2 ~ 0))
  df
}

aif_article <- map_df(aif_article, read_my_data)
aif_reply <- map_df(aif_reply, read_my_data)
echo_article <- map_df(echo_article, read_my_data)
echo_reply <- map_df(echo_reply, read_my_data)


aif_article %<>% mutate(press = "aif")
aif_reply %<>% mutate(press = "aif")
echo_article %<>% mutate(press = "echo")
echo_reply %<>% mutate(press = "echo")

article <- rbind(aif_article, echo_article)
reply <- rbind(aif_reply, echo_reply)

article$date %<>% lubridate::as_date()
reply$date %<>% lubridate::as_date()

article %>% group_by(date, press) %>% summarise(sentiment = mean(sentiment) * 100) -> res_article
reply %>% group_by(date, press) %>% summarise(sentiment = mean(sentiment) * 100) -> res_reply

write_csv(res_article, "res_article.csv")
write_csv(res_reply, "res_reply.csv")

res_article %>% ggplot(aes(x = date, y = sentiment)) + geom_line()


plot_article_tone <-
  ggplot(res_article, aes(x = date, y = sentiment, color = press)) +
  geom_line(size = 1) +
  scale_colour_viridis(discrete=TRUE, ) +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(angle = 90),
        legend.position = 'bottom') +
  labs(title = "Average Tone Ariticle By Press", x = "Time", y = "Tone") +
  scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = '1 months')

plot_article_tone

plot_reply_tone <-
  ggplot(res_reply, aes(x = date, y = sentiment, color = press)) +
  geom_line(size = 1) +
  scale_colour_viridis(discrete=TRUE, ) +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(angle = 90),
        legend.position = 'bottom') +
  labs(title = "Average Tone Reply By Press", x = "Time", y = "Tone") +
  scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = '1 months')

plot_reply_tone
