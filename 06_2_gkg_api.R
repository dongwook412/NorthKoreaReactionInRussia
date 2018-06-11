library(glue)
library(jsonlite)
library(httr)

# get gkg tone -----------
get_gdelt_gkg_tone <-
  function(term,
           source_languages = 'rus',
           start_date,
           end_date) {
    term <- URLencode(term)
    url <-
      glue(
        'https://api.gdeltproject.org/api/v2/doc/doc?query={term}%20sourcelang:{source_languages}&mode=timelinetone&format=json&startdate={start_date}&enddate={end_date}&maxrecords=250&sort=dateasec'
      )
    get <- GET(url)
    get
  }

# get gkg artlist --------
get_gdelt_gkg_at <-
  function(term,
           source_languages = 'rus',
           start_date,
           end_date) {
    term <- URLencode(term)
    url <-
      glue(
        'https://api.gdeltproject.org/api/v2/doc/doc?query={term}%20sourcelang:{source_languages}&mode=artlist&format=json&startdate={start_date}&enddate={end_date}&maxrecords=250&sort=dateasec'
      )
    get <- GET(url)
    ret <- content(get, 'text')
    ret
  }

# collect artlist ------------
collet_data_artlist <- function(str_vec) {
  li <- list()
  len <- length(str_vec)
  for (i in 1:len) {
    startdate <- str_split(str_vec[i], ' - ')[[1]][1] %>%
      str_remove_all('-') %>%
      str_remove_all(':') %>%
      str_remove_all(' ')
    
    enddate <- str_split(str_vec[i], ' - ')[[1]][2] %>%
      str_remove_all('-') %>%
      str_remove_all(':') %>%
      str_remove_all(' ')
    
    li[[i]] <-
      get_gdelt_gkg_at("north korea", start_date = startdate, end_date = enddate) %>% fromJSON() %>% `$`(articles)
    Sys.sleep(1)
  }
  res <- li %>% map_df(as.data.frame) %>% unique()
  res
}


# collect data -----------
month <-
  generate_dates("2018-05-20", "2018-06-21", time_interval = 'weeks')

res <- collet_data_artlist(month)

