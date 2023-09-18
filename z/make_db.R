#' @name make_db.R
#' @description Summary of how the miniature database z/db.sqlite is made
#' @note This code only works on Tim's computer

library(DBI)
library(RSQLite)
library(dplyr)
path = "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_static/db/data.sqlite"
con = dbConnect(RSQLite::SQLite(), path)
getwd()

db = dbConnect(RSQLite::SQLite(), "z/db.sqlite")

# Geoids for Tompkins County and Cortland County
geoids = c("d36109", "d36023")
for(i in geoids){
  con %>%
    tbl(i) %>%
    collect()  %>%
    dbWriteTable(conn = db, name = i, value = ., overwrite = TRUE, append = FALSE)
}

dbDisconnect(db)
dbDisconnect(con)

rm(list = ls())


# Repeat for covariates database


path = "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_static/db/cov.sqlite"
con = dbConnect(RSQLite::SQLite(), path)
getwd()

db = dbConnect(RSQLite::SQLite(), "z/cov.sqlite")

# Geoids for Tompkins County and Cortland County
geoids = c("areas", "c36109", "c36023")
for(i in geoids){
  con %>%
    tbl(i) %>%
    collect()  %>%
    dbWriteTable(conn = db, name = i, value = ., overwrite = TRUE, append = FALSE)
}

dbDisconnect(db)
dbDisconnect(con)

rm(list = ls())
# Repeat for covariates database



# Make CSVs
source("R/connect.R")
db = connect("data")
library(dplyr)
library(readr)
db %>% tbl("d36109") %>%
  collect() %>%
  write_csv("z/d36109.csv")
dbDisconnect(db)
db = connect("cov")
db %>% tbl("c36109") %>%
  collect() %>%
  write_csv("z/c36109.csv")
dbDisconnect(db)
rm(list = ls())

