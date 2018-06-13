library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(glue)
library(ggplot2)
library(scales)
library(magrittr)
library(viridis)


# Event Number of Articles ----------

# Read data ----------

avg_tone <- read_csv('Result/GDELT/EVENT/tone/avg_tone.csv')
colnames(avg_tone) <- c("country", "date", "avg_tone")
str_kordate <- function(string) {
  res <- string %>%
    str_replace(pattern = "년 ", "-") %>% 
    str_replace(pattern = "월 ", "-") %>% 
    str_replace(pattern = "일", "")
  res
}
avg_tone$date %<>% str_kordate()


# Cut off by month ---------

year_month <- function(date) {
  year <- year(date)
  month <- month(date)
  glue("{year(date)}-{month(date)}-1")
}

avg_tone$year_month <- ymd(purrr::map_chr(avg_tone$date, year_month))
avg_tone_month <-
  avg_tone %>% group_by(year_month) %>% summarise(mean_tone = mean(avg_tone))

avg_tone_rus <- avg_tone %>% filter(country == "RUS")
avg_tone_rus$year_month <-
  ymd(purrr::map_chr(avg_tone_rus$date, year_month))
avg_tone_rus_month <-
  avg_tone_rus %>% group_by(year_month) %>% summarise(mean_tone = mean(avg_tone))


# Avg Tone ---------------------

avg_tone_month$country <- "WORLD"
avg_tone_rus_month$country <- "RUSSIA"

plot_data <- rbind(avg_tone_month, avg_tone_rus_month)


plot_tone <-
  ggplot(plot_data, aes(x = year_month, y = mean_tone, color = country)) +
  geom_line(size = 1) +
  scale_colour_viridis(discrete=TRUE, ) +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(angle = 90),
        legend.position = 'bottom') +
  labs(title = "Average Tone Against North Korea", x = "Time", y = "Tone") +
  scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = '1 months')

diff <- avg_tone_rus_month %>%
  left_join(avg_tone_month, by = 'year_month') %>%
  select(year_month, mean_tone.x, mean_tone.y) %>%
  mutate(diff = mean_tone.x - mean_tone.y)

plot_tone_diff <- ggplot(diff, aes(x = year_month, y = diff)) +
  geom_line(size = 1) +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(angle = 90)) +
  labs(title = "Difference in tone against North Korea\nbetween Russia and World",
       x = "Time", y = "Difference") + 
  scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = '1 months')


plot_tone
plot_tone_diff
