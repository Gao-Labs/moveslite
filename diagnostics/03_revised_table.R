#' @name 03_revised_table
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
  select(formula, formula_id) %>%
  distinct() %>%
  collect() %>%
  View()


db %>%
  tbl("samples") %>%
  filter(formula_id >= 11) %>%
  summarize(count = n())

db %>%
  tbl("samples") %>%
  filter(formula_id >= 11)

ex = db %>%
  tbl("samples") %>%
  filter(formula_id >= 11) %>%
  group_by(formula_id, formula, by) %>%
  summarize(adjr = median(adjr, na.rm = TRUE)) %>%
  collect() %>%
  tidyr::pivot_wider(id_cols = c(formula_id, formula), names_from = by, values_from = adjr)

ex %>% write_csv("diagnostics/table1.csv")

# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

# Can you...
# - Make this into a really nice Table 1
# - Repeat this for the six criterion polluants in a Table 2: CO, SO2, VOCC, CO2e, PM10, PM2.5
# - Estimates for the range of predictive accuracy? Eg. median sigma, or 95% quantile range around adjr?
# Highlight on our final prefer formula, with an explanation for why it's good
# - theoretical sense, high r2, and works for many situations




dbDisconnect(db)

# Best recent models.

# BEST
# "log(emissions)~poly(year, 2) + poly(log(vmt), 3) +
# vehicles + sourcehours + starts"

# "log(emissions)~poly(year, 2) + poly(log(vmt), 3) +
#vehicles + sourcehours"

# Simple Enough
# "log(emissions)~year + poly(log(vmt), 2) +
#sqrt(vehicles) + sqrt(sourcehours)"

# "log(emissions)~year + poly(log(vmt), 2) +
#log(vehicles) + sqrt(sourcehours)"




