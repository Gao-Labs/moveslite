#' @name 03_revised_table
#' @description Investigate results of `diagnostics.sqlite` from `02_diagnostic_loop.R`
#' @author Yan Guo and Tim Fraser


# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

# Can you...
# - Make this into a really nice Table 1
# - Repeat this for the six criterion polluants in a Table 2: CO, SO2, VOCC, CO2e, PM10, PM2.5
# - Estimates for the range of predictive accuracy? Eg. median sigma, or 95% quantile range around adjr?
# Highlight on our final prefer formula, with an explanation for why it's good
# - theoretical sense, high r2, and works for many situations

# 0. Model Fit #####################################################

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")


criteria = tribble(
  ~id, ~name,
  16, "Overall",
  8, "By sourcetype",
  14, "By fueltype"
)

# What percentage of models for each by exceed these thresholds?
# Among all runs, count the number of geoid-sets where the explanatory power is...
stat_by = db %>%
  tbl("samples") %>%
  filter(by %in% !!criteria$id) %>%
  group_by(formula_id, formula, by) %>%
  summarize(
    mean = mean(adjr, na.rm = TRUE),
    sd = sd(adjr, na.rm = TRUE),
    count = n(),
    excellent = sum(adjr >= 0.99, na.rm = TRUE),
    success = sum(adjr >= 0.95 & adjr < 0.99, na.rm = TRUE),
    fine = sum(adjr >= 0.90 & adjr < 0.95, na.rm = TRUE),
    warning = sum(adjr >= 0.80 & adjr < 0.90, na.rm = TRUE),
    danger = sum(adjr < 0.80, na.rm = TRUE)
    ) %>%
  mutate(se = sd / sqrt(count),
         bad = warning + danger) %>%
  ungroup() %>%
  collect() %>%
  mutate(across(.cols = c(danger, warning, fine, success, excellent, bad),
                .fns = ~paste0(round(.x / count*100, 0), "%")),
         mean = round(mean, 3),
         # If true standard error is less than 0.001, round it up to 0.001, to be conservative when reporting the table
         se = if_else(se < 0.001, true= 0.001, false = se),
         se = round(se, 3),
         mean = paste0(round(mean * 100, 1), "%"),
         se = paste0(round(se*100, 1), "%")) %>%
  # mutate(by = by %>% dplyr::recode_factor(
  #   "16" = "Overall",
  #   "14" = "By Fueltype",
  #   "8" =  "By Source")) %>%
  # mutate(by = forcats::fct_relevel(by, c("Overall", "By Fueltype", "By Source"))) %>%
  arrange(formula_id, by) %>%
  # Add back in emissions at the front
  mutate(formula = if_else(condition = formula_id < 11, true = paste0("emissions ~ ", formula), false = formula)) %>%
  select(formula_id, formula, by, mean, se, bad, count) %>%
  tidyr::pivot_wider(id_cols = c(formula_id, formula), names_from = by, values_from = c(mean, se, count, bad)) %>%
  select(id = formula_id, formula,
         mean_16, se_16, bad_16, count_16,
         mean_8, se_8, bad_8, count_8,
         mean_14, se_14, bad_14, count_14) %>%
  readr::write_csv("diagnostics/table1.csv")


dbDisconnect(db)
library(stringr)
library(knitr)
library(kableExtra)

readr::read_csv("diagnostics/table1.csv") %>%
  kable(format = "html", col.names = c("ID", "Model Equation",
                                       "Mean", "SE", "% Poor", "N",
                                       "Mean", "SE", "% Poor", "N",
                                       "Mean", "SE", "% Poor", "N")) %>%
  kable_paper(bootstrap_options = c("striped", "condensed"), full_width = FALSE) %>%
  kable_styling() %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, bold = FALSE, border_right = TRUE) %>%
  kableExtra::add_header_above(header = c(" " = 2, "Overall" = 4, "By Source" = 4, "By Fueltype" = 4)) %>%
  kableExtra::add_footnote(
    notation = "none",
    label = "Mean shows mean adjusted R-squared over N models, \nwith a standard error of SE.\n% Poor shows percentage of models with poor fit, meaning less than 90% accuracy.") %>%
  cat(file = "diagnostics/table1.html")
  save_kable(file = "diagnostics/table1.png", density = 300)

browseURL("diagnostics/table1.png")



# 3. Table of 6 pollutants  #################################
# CO, SO2, VOCC, CO2e, PM10, PM2.5 with the best model (formula id == 51)


library(dplyr)
library(DBI)
library(ggplot2)
library(knitr)
library(kableExtra)

db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")

criteria = tribble(
  ~id, ~name,
  98, "CO2e",
#  87, "VOC",
  100, "PM10",
  110, "PM2.5",
  31, "SO2",
  6, "N20",
  2, "CO"
) %>%
  # Filter out VOC. I don't have confidence in VOC currently.
  filter(id != 87)

# What is the median / rank for each formula OVERALL?
stat_overall = db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  group_by(formula_id, formula) %>%
  summarize(stat = median(adjr, na.rm = TRUE),
            valid = sum(adjr >= 0.90, na.rm = TRUE),
            test = n()
  ) %>%
  collect()

stat_overall = stat_overall %>%
  mutate(valid = valid/test) %>%
  ungroup() %>%
  mutate(valid = paste0(round(valid*100, 0), "%")) %>%
  arrange(desc(stat)) %>%
  mutate(rank = 1:n()) %>%
  subset(select = -c(test) )

best_overall = stat_overall %>% filter(rank == 1)

# What percentage of models for each pollutant exceed these thresholds?
# Among all runs, count the number of geoid-sets where the explanatory power is...
values = db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  filter(formula_id == !!best_overall$formula_id) %>%
  collect()

# Booststrap some confidence intervals
n_reps = 1000

# Get bootstrapped confidence intervals!
cis = values %>%
  select(formula_id, pollutant, adjr) %>%
  tidyr::expand_grid(rep = 1:n_reps) %>%
  # Resample with replacement
  group_by(rep, pollutant) %>%
  sample_n(size = n(), replace = TRUE) %>%
  ungroup() %>%
  # Calculate stat
  group_by(rep, pollutant) %>%
  summarize(stat = median(adjr, na.rm = TRUE)) %>%
  ungroup() %>%
  # Calculate most frequent 95% range
  group_by(pollutant) %>%
  summarize(lower = quantile(stat, probs = 0.025, na.rm = TRUE),
            upper = quantile(stat, probs = 0.975, na.rm = TRUE)) %>%
  ungroup()


s = values %>%
  group_by(pollutant) %>%
  summarize(stat = median(adjr, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(by = "pollutant", y = cis) %>%
  mutate(pollutant_name = pollutant %>% dplyr::recode_factor(!!!purrr::set_names(criteria$name, criteria$id))) %>%
  mutate(across(.cols = c(stat, lower, upper),
                .fns = ~round(.x*100, 1))) %>%
  mutate(label = paste0(stat, "%"))

tab = s %>%
  select(pollutant_name, label, lower, upper, stat)

tab %>% readr::write_csv("diagnostics/table_cis.csv")


rm(list = ls())

library(knitr)
library(kableExtra)

tab = readr::read_csv("diagnostics/table_cis.csv")

tab %>%
  select(-stat) %>%
  kable(format = "html", col.names = c("Pollutant", "Median", "2.5% CI", "97.5% CI")) %>%
  kable_paper(bootstrap_options = c("striped", "condensed"), full_width = FALSE) %>%
  kable_styling() %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  kableExtra::add_header_above(header = c(" ", "Expected Accuracy Range\nfor Best Model (Overall)" = 3)) %>%
  kableExtra::add_footnote(
    notation = "none",
    label = "Statistics depict adjusted R-squared, \n with bootstrapped confidence intervals (n = 1000 reps).") %>%
  # column_spec(2, image = spec_pointrange(
  #   x = tab$stat,
  #   xmin = tab$lower,
  #   xmax = tab$upper,
  #   vline = 100)) %>%
  cat(file = "diagnostics/table_cis.html")
  save_kable(file = "diagnostics/table_cis.png", density = 300)

browseURL("diagnostics/table_cis.png")
dbDisconnect(db); rm(list = ls()); gc()


# 4. Correlation #########################

library(dplyr)
library(DBI)
library(broom)
library(stringr)
source("R/connect.R")
db = connect("data")

# Total counties covered in our assessment
tibble(table = db %>% dbListTables() ) %>%
  filter(nchar(table) == 6) %>%
  summarize(count = n())

# Total Counties in US
tigris::fips_codes %>%
  count()

# Total states covered in our county assessment
tibble(table = db %>% dbListTables() ) %>%
  filter(nchar(table) == 6) %>%
  mutate(state = str_sub(table, 1,3)) %>%
  select(state) %>%
  distinct() %>%
  count()

# FIPS code for New York County (Manhattan) is 36061
tigris::fips_codes %>%
  filter(county == "New York County")

.geoid = "36061"
dat = db %>%
  tbl(paste0("d",.geoid)) %>%
  filter(pollutant == 98, year == 2020, geoid == !!.geoid) %>%
  collect()

row1 = dat %>% filter(by == 16) %>% mutate(aggregation = "Overall", category = "Overall")
row2 = dat %>% filter(by == 8 & sourcetype == 21) %>% mutate(aggregation = "By Source", category = "Passenger Vehicles")
row3 = dat %>% filter(by == 14 & fueltype == 1) %>% mutate(aggregation = "By Fuel Type", category = "Gasoline")


tab = bind_rows(row1, row2, row3, .id = "subset") %>%
  mutate(subset = paste0("subset_", subset)) %>%
  select(subset, year, geoid, pollutant, aggregation, category, emissions:starts) %>%
  mutate(across(.cols = c(emissions:starts), .fns = ~scales::number(.x, decimal.mark = ".", big.mark = ","))) %>%
  tidyr::pivot_longer(
    cols = -c(subset),
    names_to = "var", values_to = "value",
    values_transform = list(value = as.character)) %>%
  tidyr::pivot_wider(id_cols = var, names_from = subset, values_from = value) %>%
  mutate(unit = case_when(var == "emissions" ~ "tons",
                          var == 'vmt' ~ "miles",
                          var == "vehicles" ~ "vehicles",
                          var == "sourcehours" ~ "hours",
                          var == "starts" ~ "times",
                          var == "year" ~ "years",
                          TRUE ~ "-")) %>%
  mutate(label = var %>% dplyr::recode_factor(
    "geoid" = "County FIPS Code",
    "pollutant" = "Pollutant ID",
    "aggregation" = "Aggregation Level",
    "category" = "Category",
    "emissions" = "Emissions",
    "vmt" = "Vehicle Miles Traveled",
    "vehicles" = "Vehicle Population",
    "sourcehours" = "Source Hours Driven",
    "starts" = "Vehicle Starts",
    "year" = "Year"
    )) %>%
  arrange(label) %>%
  select(label, unit, var, contains("subset_"))

tab %>%
  kable(format = "html", col.names = c("Variable", "Unit",  "Abbreviation",  "Subset A", "Subset B", " Subset C"),
        align = c("l", "c", "l", "c", "c", "c")) %>%
  kable_paper(bootstrap_options = c("striped", "condensed"), full_width = FALSE) %>%
  kable_styling() %>%
  column_spec(1, bold = FALSE, italic = FALSE, border_right = TRUE) %>%
  column_spec(2, bold = FALSE, italic = TRUE, border_right = TRUE) %>%
  column_spec(3, bold = FALSE, italic = FALSE, border_right = TRUE) %>%
  kableExtra::group_rows(group_label = "Identifiers", start_row = 1, end_row = 2) %>%
  kableExtra::group_rows(group_label = "Subset Traits", start_row = 3, end_row = 4) %>%
  kableExtra::group_rows(group_label = "Metrics", start_row = 5, end_row = 9) %>%
  kableExtra::group_rows(group_label = "Time (14 years per subset)", start_row = 10, end_row = 10) %>%
  kableExtra::add_header_above(header = c("MOVES Subset Variables" = 3, "1 Year, 3 Example Subsets" = 3)) %>%
  kableExtra::add_footnote(
    notation = "none",
    label = paste0(
      "* Metrics rounded in table for visual clarity.",
      "** Each subset refers to CO2 Equivalent emissions in New York County (Manhattan) in 2020.",
      "*** We define 1 subset as a county-pollutant-aggregation-category set. 1 subset has t = 14 years.",
      collapse = "\n ")
    ) %>%
  cat(file = "diagnostics/table_example.html")

browseURL("diagnostics/table_example.html")


dbDisconnect(db)

db = DBI::dbConnect(drv = RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")


criteria = tribble(
    ~id, ~name,
    98, "CO2e",
    87, "VOC",
    100, "PM10",
    110, "PM2.5",
    31, "SO2",
    6, "NO2",
    2, "CO"
  ) %>%
  # Filter out VOC. I don't have confidence in VOC currently.
  filter(id != 87)

db %>%
  tbl("runs") %>%
  filter(pollutant %in% !!criteria$id) %>%
  count()



db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  filter(by ==  8 | by == 16 | by == 14) %>%
  count()

db %>%
  tbl("samples") %>%
  select(geoid) %>%
  distinct() %>%
  count()

db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  group_by(pollutant, set_id) %>%
  count()

# For each subset we took a random sample of counties

db %>%
  tbl("samples") %>%
  filter(pollutant %in% !!criteria$id) %>%
  group_by(pollutant, set_id, formula_id) %>%
  count() %>%
  ungroup() %>%
  summarize(mean = mean(n))
