# setup.R

# Run this script to get you setup for the first time.

# Remove catr if it is already installed
remove.packages("catr")
# Install catr from source
install.packages("z/catr_0.1.0.tar.gz", type = "source")

# Connect to sqlite
db = dbConnect(RSQLite::SQLite(), "z/db.sqlite")

# Test table 
t = "d36109"

db %>% 
  tbl(t) %>%
  head() %>%
  collect()


dbDisconnect(db); rm(list = ls())
