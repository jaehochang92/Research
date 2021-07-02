library(shiny)
library(shinySignals)
library(shinydashboard)
library(RMySQL)
library(pool)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)
now2 <- function() {
  as_datetime(Sys.time()) + hours(9)
}
my_db <- dbPool(
  MySQL(),
  dbname = 'fitbit',
  username = 'fitbit_admin',
  password = 'q1234',
  host = '1.234.23.50',
  port = 3306
)
dbl <- dbListTables(my_db)
master.tb <-
  filter(tbl(my_db, 'fitbit'),!is.na(regdate)) %>%
  arrange(desc(regdate))
users <- as_tibble(distinct(select(master.tb, userid), userid))
Vars <-
  colnames(select(master.tb, -c(idx, userid, usernum, username, regdate)))

auto_since <- function(master.tb, prd) {
  # now2() -
  master.tb %>% summarise(mr = max(regdate, na.rm = T)) %>%
    select(mr) %>% as_tibble(.) %>% .[[1]] %>%
    as_datetime() - period(hours = prd[1],
                           minutes = prd[2],
                           seconds = prd[3])
}