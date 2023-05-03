# make_db.R

# This code only works on Tim's computer

library(DBI)
library(RSQLite)
library(dplyr)
path = "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_static/db/data.sqlite"
con = dbConnect(RSQLite::SQLite(), path)
getwd()

db = dbConnect(RSQLite::SQLite(), "z/db.sqlite")

con %>% 
  tbl("d36109") %>%
  collect()  %>%
  dbWriteTable(conn = db, name = "d36109", value = ., overwrite = TRUE, append = FALSE)

dbDisconnect(db)
dbDisconnect(con)

rm(list = ls())
