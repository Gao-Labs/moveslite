# For replication purposes, we're going to make the CATSERVER table for Tompkins County available as a sqlite file

library(DBI)
library(RMySQL)
library(RSQLite)

# Load environmental variables - only the authors have these
readRenviron(".Renviron")
# Load function for connecting to CATSERVER, using those variables
source("R/connect.R")

# Initialize SQLite Database
data = dbConnect(drv = RSQLite::SQLite(), "diagnostics/case_study.sqlite")

# Connect to the 'data' database (tenatively your z/db.sqlite file)
db = connect("granddata")

# Collect data from CATSERVER
d36109 = db %>% tbl("d36109") %>% collect()

# Write data to case_study.sqlite
data %>% dbWriteTable(conn = ., name = "d36109", value = d36109)

# Disconnect
dbDisconnect(data)

dbDisconnect(db)
rm(list = ls())


# You can now access the case study data using case_study.sqlite
