# generate_date

library(stringr)
library(purrr)

generate_ymd_hms <- function(date) {
  if (date %>% stringr::str_detect(":")) {
    date <- date %>% lubridate::ymd_hms() %>% force_tz()
    return(date)
  }
  glue::glue("{date} 12:00:00") %>% as.character() %>% lubridate::ymd_hms() %>% lubridate::force_tz(tzone = 'UTC')
}


generate_dates <-
  function(start_date = NULL,
           end_date = NULL,
           time_interval = "days") {
    
    if (time_interval %>% purrr::is_null()) {
      time_interval <- 'hours'
    }
    time_interval <- time_interval %>% str_to_lower()
    period_types <- c("months","weeks", 'days', 'hours', 'minutes', "year")
    if (!time_interval %in% period_types) {
      stop(glue::glue("Sorry period types can on only be {str_c(period_types, collapse = ', ')}") %>% as.character())
    }
    
    now <- Sys.time()
    earliest <- now - lubridate::dweeks(12)
    if (end_date %>% purrr::is_null()) {
      end_date <- now
    } else {
      end_date <- end_date %>% generate_ymd_hms()
    }
    if (start_date %>% purrr::is_null()) {
      start_date <- earliest
    } else {
      start_date <-
        start_date %>% generate_ymd_hms()
    }
    
    all_dates <-
      seq(start_date, end_date, time_interval)
    
    end_seq <- length(all_dates) - 1
    
    dates <-
      1:end_seq %>%
      map_chr(function(x) {
        start <-
          all_dates[[x]] %>% as.character()
        end <-
          (all_dates[[x + 1]] - lubridate::dseconds(1)) %>% as.character()
        str_c(start, end, sep = " - ")
      })
    dates
  }


generate_dates("2018-01-01", "2018-02-01", time_interval = 'days') -> k
