#' @name 02_investigation.R
#' @description Investigate results of `diagnostics.sqlite` from `02_diagnostic_loop.R`

library(dplyr)
library(DBI)
library(RSQLite)
library(stringr)
db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")

db %>%
  dbListTables()

db %>%
  tbl("samples") %>%
  filter(formula_id > 40)


db %>%
  tbl("samples") %>%
  filter(run_id == max(run_id))

db %>%
  tbl("samples") %>%
  filter(formula_id == 44)

dat =  db %>%
  tbl("samples") %>%
  filter(formula_id > 40) %>%
  group_by(formula_id, formula) %>%
  summarize(adjr = median(adjr, na.rm = TRUE)) %>%
  arrange(desc(adjr)) %>%
  collect()

dat$formula

dat %>%
  filter(str_detect(formula, "log[(]emissions[)].*.log[(]vmt[)]")) %>%
  filter(str_detect(formula, "poly[(]", negate = TRUE))


db %>%
  tbl("samples") %>%
  filter(formula_id == max(formula_id))

dbDisconnect(db)

# Best recent models.

# "log(emissions)~poly(year, 2) + poly(log(vmt), 3) +
# vehicles + sourcehours + starts"

# "log(emissions)~poly(year, 2) + poly(log(vmt), 3) +
#vehicles + sourcehours"

# "log(emissions)~year + poly(log(vmt), 2) +
#sqrt(vehicles) + sqrt(sourcehours)"

# "log(emissions)~year + poly(log(vmt), 2) +
#log(vehicles) + sqrt(sourcehours)"
