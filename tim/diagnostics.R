# diagnostics
# May 22 - for paper


setwd("C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/moveslite")
getwd()
source("R/connect.R")

db = connect("data")
db %>% tbl("d36109") %>% collect() %>% nrow()
db %>% tbl("d36109") %>% select(pollutant) %>% distinct() %>% collect() %>% nrow()
db %>% tbl("d36109") %>% select(sourcetype) %>% distinct() %>% collect()
db %>% tbl("d36109") %>% select(fueltype) %>% distinct() %>% collect()
db %>% tbl("d36109") %>% select(roadtype) %>% distinct() %>% collect()
db %>% tbl("d36109") %>% select(regclass) %>% distinct() %>% collect()


path ="C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_dashboard/data/core.rds"
path %>% read_rds() %>%
  .$choices %>%
  .$pollutant

21 * (4 + 4 + 9 + 13 + 1)
651/21
