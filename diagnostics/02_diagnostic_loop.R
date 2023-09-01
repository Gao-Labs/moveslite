#' @name 02_diagnostic_loop.R
#' @title Diagnostic Loop
#' @author Tim, Yan
#' @description Diagnostic Workflow for all Models for all Pollutants

# Use the commented out code to run this script as a background job.
# rstudioapi::jobRunScript(name = "diagnostic", path = "diagnostics/02_diagnostic_loop.R", workingDir = "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/moveslite")

# Clear environment and cache
rm(list = ls()); gc()

# Load packages
library(dplyr)
library(broom)
library(readr)
library(purrr)
library(DBI)
library(RSQLite)

# These are the 5 core functions in `moveslite`.
source("R/connect.R")
source("R/query.R")
source("R/estimate.R")
source("R/diagnose.R")
source("R/diagnose_many.R")

#' @note List of formulas we have theoretical and empirical reasons to expect will perform extremely well.
# formulas <- list(
#   "1" =  emissions ~ poly(vmt, 2) + poly(vehicles,2) + poly(sourcehours, 2) + poly(starts,2) + year,
#   "2"  =  emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours, 2) + starts + year,
#   "3"  =  emissions ~ year + vmt + sourcehours + vehicles + starts +
#     year*vmt + year*sourcehours + vmt*sourcehours,
#   "4"  =  emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours,2) + starts + year +
#     year*vmt + year*sourcehours + vmt*sourcehours,
#   "5"  =  emissions ~ poly(vmt, 2) + poly(vehicles, 2) + poly(sourcehours, 2) + + starts + year,
#   "6"  =  emissions ~ poly(vmt, 2) + poly(vehicles,2) + sourcehours + poly(starts, 2) + year,
#   "7"  =  emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours, 2) + poly(starts, 2) + year,
#   "8"  =  emissions ~ vmt + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year,
#   "9"  =  emissions ~ sqrt(vmt) + sqrt(vehicles) + sqrt(sourcehours) + sqrt(starts) + year,
#   "10" =  emissions ~ poly(vmt, 3) + year + vmt*year
# )
# Not all regulatory classes or roadtypes or fueltypes actually exist in every county,
# meaning that some runs won't actually produce any results. That's okay.


# formulas <- list(
#   # Simple single-variable Model
#   "11" = log(emissions) ~ vmt + year,
#   "12" = log(emissions) ~ log(vmt) + year,
#   # With interaction
#   "13" = log(emissions) ~ vmt * year,
#   "14" = log(emissions) ~ log(vmt)*year,
#   # Try polynomials of single-variable
#   "15" =  log(emissions) ~ year + poly(vmt, 2), # second degree
#   "16" =  log(emissions) ~ year * poly(vmt, 2), # with interaction
#   "17" =  log(emissions) ~ year + poly(log(vmt), 2), # with log
#   "18" =  log(emissions) ~ year * poly(log(vmt), 2), # with interaction
#   "19" =  log(emissions) ~ year + poly(vmt, 3), # third degree
#   "20" =  log(emissions) ~ year * poly(vmt, 3), # with interaction
#   "21" =  log(emissions) ~ year + poly(log(vmt), 3), # with log
#   "22" =  log(emissions) ~ year * poly(log(vmt), 3), # with interaction
#   # Simple 2-variable model
#   "23" = log(emissions) ~ year + vmt + vehicles,
#   "24" = log(emissions) ~ year + log(vmt) + log(vehicles),
#   "25" = log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + year,
#   "26" = log(emissions) ~ poly(log(vmt), 2) + poly(log(vehicles), 2) + year,
#   # Simple 3-variable model
#   "27" = log(emissions) ~ year + vmt + vehicles + sourcehours,
#   "28" = log(emissions) ~ year + log(vmt) + log(vehicles) + log(sourcehours),
#   "29" = log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + log(sourcehours) + year,
#   "30" = log(emissions) ~ poly(log(vmt), 2) + poly(log(vehicles), 2) + log(sourcehours) + year,
#   "31" = log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + poly(log(sourcehours),2) + year,
#   "32" = log(emissions) ~ poly(log(vmt), 2) + poly(log(vehicles), 2) + poly(log(sourcehours),2) + year,
#   # 4-variable model
#   "33" = log(emissions) ~ year + vmt + vehicles + sourcehours + starts,
#   "34" = log(emissions) ~ year + log(vmt) + log(vehicles) + log(sourcehours) + log(starts),
#   "35" = log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + log(sourcehours) + log(starts) + year,
#   "36" = log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + poly(log(sourcehours),2) + log(starts) + year,
#   "37" = log(emissions) ~ poly(log(vmt), 2) + poly(log(vehicles),2) + log(sourcehours) + log(starts) + year,
#   "38" = log(emissions) ~ poly(log(vmt), 2) + poly(log(vehicles), 2) + poly(log(sourcehours),2) + log(starts) + year,
#   # Crazy Interactions
#   "39"  =  log(emissions) ~ poly(vmt, 2) + vehicles + poly(sourcehours,2) + starts + year +
#        year*vmt + year*sourcehours + vmt*sourcehours,
#   "40"  =  log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + poly(log(sourcehours),2) + log(starts) + year +
#     year*log(vmt) + year*log(sourcehours) + log(vmt)*log(sourcehours)
# )

formulas = list(
  # Really good version
  #"41" = log(emissions) ~ year + poly(log(vmt), 2) + log(vehicles) + sqrt(sourcehours),
  #"42" = log(emissions) ~ year + poly(log(vmt), 2) + sqrt(vehicles) + sqrt(sourcehours),

  "43" = log(emission) ~ poly(year, 2) + poly(log(vmt), 2) + log(vehicles) + sqrt(sourcehours),
  "44" = log(emission) ~ poly(year, 2) + poly(log(vmt), 2) + sqrt(vehicles) + sqrt(sourcehours),

  "45" = log(emission) ~ year + log(vmt) + log(vehicles) + sqrt(sourcehours),
  "46" = log(emission) ~ year + log(vmt) + sqrt(vehicles) + sqrt(sourcehours),

  "47" = log(emission) ~ poly(year, 2) + log(vmt) + log(vehicles) + sqrt(sourcehours),
  "48" = log(emission) ~ poly(year, 2) + log(vmt) + sqrt(vehicles) + sqrt(sourcehours),

  "49" = log(emission) ~ poly(year, 2) + poly(log(vmt), 3) + sqrt(vehicles) + sqrt(sourcehours)

#  "50" = log(emissions) ~ poly(year, 2) + poly(log(vmt), 3) + vehicles + sourcehours,
#  "51" = log(emissions) ~ poly(year, 2) + poly(log(vmt), 3) + vehicles + sourcehours + starts
)



conn = dbConnect(drv = RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")

# Make MySQL field types for our CAT FORMATTED data
# Must be supplied as a named vector
fieldtypes = c(
  run_id = "smallint(7)",
  geoid = "char(5)",
  set_id = "smallint(4)",
  pollutant = "tinyint(3)",
  by = "tinyint(2)",
  type = "tinyint(2)",
  formula_id = "tinyint(2)",
  formula = "char",
  adjr = "decimal(5,2)",
  sigma = "double(18,1)",
  df.residual = "tinyint(2)")

# Get list of current tables
tables = conn %>% dbListTables()

# If SQLite table 'runs' is NOT yet initialized
if(!"samples" %in% tables){
  # Make a template entry
  template = diagnose_many(.geoid = "36109", .pollutant = 98, .by = 8, .type = 42, .formulas = list("1" = emissions ~ vmt)) %>%
    mutate(run_id = 1, set_id = 1) %>%
    # Arrange any columns that may exist in this order, if they exist
    select(any_of(c("run_id", "geoid",  "set_id", "pollutant", "by", "type", "formula_id", "formula", "adjr", "sigma", "df.residual"))) %>%
    slice(0)
  # Initialize the table
  conn %>% dbWriteTable(name = "samples", value = template, field.types = fieldtypes)
}

# Check the run_id last completed
# latest = conn %>% tbl("samples") %>% summarize(run_id = max(run_id, na.rm = TRUE)) %>% collect() %>% with(run_id)
# latest = if(is.na(latest)){ 0 }else{ latest }

#latest = conn %>% tbl("samples") %>% filter(formula_id > 40) %>% summarize(run_id = max(run_id, na.rm = TRUE)) %>% collect() %>% with(run_id)
#latest = if(is.na(latest)){ 0 }else{ latest }
latest = 0

# Disconnect
dbDisconnect(conn)

runs = read_rds("diagnostics/runs_sample.rds") %>%
  # Filter to just NEW runs we have not yet written to the database.
  filter(run_id > !!latest)

# Get total runs left
n = nrow(runs)

# Setup parallel processing
library(furrr)
library(future)
# Get number of available cores
ncores = availableCores() - 2
# Initiate parallel processing
plan(multicore, workers = ncores)

# Generate function for looping, for the ith row in 'runs'
f = function(i, runs, n){
  # Run function
  output = diagnose_many(.geoid = runs$geoid[i], .pollutant = runs$pollutant[i],
                         .by = runs$by[i], .type = runs$type[i],
                         .formulas = formulas) %>%
    # Receive a run_id
    mutate(run_id = runs$run_id[i], set_id = runs$set_id[i]) %>%
    # Arrange any columns that may exist in this order, if they exist
    select(any_of(c("run_id", "geoid", "set_id", "pollutant", "by", "type", "formula_id", "formula", "adjr", "sigma", "df.residual")))

  # If there are ANY results
  if(nrow(output) > 0){
    # Connect to database
    conn = dbConnect(drv = RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")
    # Write that output to database
    conn %>% dbWriteTable(name = "samples", value = output, append = TRUE, overwrite = FALSE)
    # Disconnect from database
    dbDisconnect(conn); remove(conn)
  }
  # Remove output
  remove(output)
  # Print Completion Message
  print(paste0("--- done: ", runs$run_id[i], "   ", paste0(round(runs$run_id[i] / nrow(runs)*100, 2), "%")));

  # Clear cache
  gc(verbose = FALSE);

}


future_map(.x = 1:n, ~f(i = .x, runs = runs, n = n),
           .progress = TRUE, .options = furrr_options(seed = 12345))

# conn = dbConnect(drv = RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")
# conn %>% tbl("samples") %>% filter(formula_id > 10)
# dbDisconnect(conn)

plan(sequential)

# Clear Environment and Cache
rm(list = ls()); gc()
# #
# library(dplyr)
# library(DBI)
# library(RSQLite)
# library(stringr)
# db = dbConnect(RSQLite::SQLite(), "diagnostics/diagnostics.sqlite")
#
# db %>%
#   dbListTables()
#
# db %>%
#   tbl("samples") %>%
#   filter(formula_id > 40)
#
#
# db %>%
#   tbl("samples") %>%
#   filter(run_id == max(run_id))
#
# db %>%
#   tbl("samples") %>%
#   filter(formula_id == 44)
#
# dat =  db %>%
#   tbl("samples") %>%
#   filter(formula_id > 40) %>%
#   group_by(formula_id, formula) %>%
#   summarize(adjr = median(adjr, na.rm = TRUE)) %>%
#   arrange(desc(adjr)) %>%
#   collect()
#
# dat$formula
#
# dat %>%
#   filter(str_detect(formula, "log[(]emissions[)].*.log[(]vmt[)]")) %>%
#   filter(str_detect(formula, "poly[(]", negate = TRUE))
#
#
# db %>%
#   tbl("samples") %>%
#   filter(formula_id == max(formula_id))
#
# dbDisconnect(db)
