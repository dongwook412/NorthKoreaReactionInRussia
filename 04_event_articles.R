library(readr)
library(lubridate)
library(ggplot2)
library(glue)
library(dplyr)
library(stringr)

articles <- read_csv('3_DataAnalyze/Press/EVENT/volume/num_of_articles.csv')
articles_rus <- read_csv('3_DataAnalyze/Press/EVENT/volume/num_of_articles_rus.csv')
articles_nk <- read_csv('3_DataAnalyze/Press/EVENT/volume/num_of_articles_nk.csv')
url <- "https://www.nti.org/documents/2137/north_korea_missile_test_database.xlsx"
destfile <- "north_korea_missile_test_database.xlsx"
download.file(url, destfile)
nk_missile <- read_excel(destfile, na = c(NA, "Unknown", "N/A"))



# Ratio

year_month <- function(date) {
  year <- year(date)
  month <- month(date)
  glue("{year(date)}-{month(date)}-1")
}

articles$year_month <- lubridate::ymd(purrr::map_chr(articles$date, year_month))
articles_month <- articles %>% group_by(year_month) %>% summarise(record = sum(record))

articles_nk_total <- articles_nk %>% group_by(date) %>% summarise(record = sum(record))
articles_nk_total$year_month <- lubridate::ymd(purrr::map_chr(articles_nk_total$date, year_month))
articles_nk_total_month <- articles_nk_total %>% group_by(year_month) %>% summarise(record = sum(record))

world_ratio <- articles_nk_total_month %>%
  left_join(articles_month, by = 'year_month') %>% 
    mutate(ratio = record.x/record.y * 1000)

articles_rus$year_month <- lubridate::ymd(purrr::map_chr(articles_rus$date, year_month))
articles_rus_month <- articles_rus %>% group_by(year_month) %>% summarise(record = sum(record))

articles_nk_rus <- articles_nk %>% filter(country == "RUS") %>% group_by(date) %>% summarise(record = sum(record))
articles_nk_rus$year_month <- lubridate::ymd(purrr::map_chr(articles_nk_rus$date, year_month))
articles_nk_rus_month <- articles_nk_rus %>% group_by(year_month) %>% summarise(record = sum(record))

russia_ratio <- articles_nk_rus_month %>% 
  left_join(articles_rus_month, by = 'year_month') %>% 
    mutate(ratio = record.x/record.y * 1000)

nk_missile_count <- nk_missile %>% group_by(Date) %>% count()
nk_missile_count$year_month <- lubridate::ymd(purrr::map_chr(nk_missile_count$Date, year_month))
nk_missile_month <- nk_missile_count %>%
  group_by(year_month) %>%
  summarise(record = sum(n)) %>% 
  filter(year_month > "2015-01-01")

world_ratio$country <- "WORLD"
russia_ratio$country <- "RUSSIA"
volume_data <- rbind(world_ratio, russia_ratio)

volume_plot <- ggplot(volume_data, aes(x = year_month, y = ratio, color = country)) +
  geom_line() +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom") + 
  labs(x = 'Time', y = '‰', title = 'Volume of Article about North Korea') +
  scale_x_date(labels = scales::date_format("%Y-%m"), date_breaks = '1 months')
# 800 * 450
volume_plot   
