# Example workflow

# setup.R

# Run this script to get you setup for the first time.

# Remove catr if it is already installed
remove.packages("catr")
# Install catr from source
install.packages("z/catr_0.1.0.tar.gz", type = "source")

# I'm going to teach you how to use catr to do custom inputs to MOVES runs - later!

library(dplyr)
library(DBI)
library(RSQLite)
library(broom)
library(readr)
library(ggplot2)
library(purrr)

# Connect to sqlite covariate db
path_cov = "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_static/db/cov.sqlite"
cov = dbConnect(RSQLite::SQLite(), path_cov)

tables = cov %>% tbl("areas") %>% filter(level == "county") %>% select(geoid) %>% collect() %>% with(geoid) %>% paste0("d", .)


# Connect to sqlite grand data db
path_data = "C:/Users/tmf77/OneDrive - Cornell University/Documents/rstudio/cat_static/db/data.sqlite"
db = dbConnect(RSQLite::SQLite(), path_data)
# Test table (Tompkins County)
t = "d36109"

db %>% 
  tbl(t) %>%
  glimpse()


# by = 8 --> aggregated by sourcetype
db %>% 
  tbl(t) %>%
  head(1)

dat = db %>% 
  tbl(t) %>%
  # Just aggregated by sourcetype, just buses
  filter(by == 8 & sourcetype == 42) %>%
  # Just CO2e emissions
  filter(pollutant == 98) %>%
  collect()

dat
dat %>%
  ggplot(mapping = aes(x = vmt, y = emissions)) + 
  geom_point()

# R2 = 0.972
m = dat %>% lm(formula = emissions ~ vmt );           glance(m)

# R2 = 0.995
m = dat %>% lm(formula = emissions ~ poly(vmt,4) );   glance(m)

# R2 = 0.976
m = dat %>% lm(formula = emissions ~ vmt + vehicles + starts + sourcehours);  glance(m)

# R2 = 0.997 -- BEST
m = dat %>% lm(formula = emissions ~ poly(vmt, 3) + vehicles + starts + sourcehours); glance(m)





# Let's take a random sample of counties and see how often we predict well.
library(dplyr)
library(broom)
library(ggplot2)

source("R/connect.R")
source("R/table_exists.R")
source("R/query.R")
source("R/estimate.R")

db = connect("data")
# Get just the full list of counties
.tables = db %>% dbListTables() %>% {.[nchar(.) == 6]} %>% stringr::str_remove("d")

.filters = c(.pollutant = 98, .by = 8, .sourcetype = 42)
# Get a model
m = estimate(.geoid = "36109", .filters = .filters)


stat = .tables %>% 
  sample(size = 10, replace = FALSE) %>%
  purrr::map_dfr(~estimate(.geoid = ., .filters = .filters) %>% broom::glance(), .id = "table")


# Paper
# - An Algorithm to Efficiently Estimate Emissions
# need: MOVES is great, but very time consuming.

# --- validation:
#     is our default moves trained model good enough at matching a custom moves run's outputs
#     users are mostly working on the x variables anyways

# (!!!) We want to retain those input x variables that counties share
#       Allow our model to... show a simple marginal effect - show the value added
#       Show the simplest model (MVP), with good accuracy
#       Show the best fit model (as many variables as possible)


# Website:
# MOVES Lite Analyzer:
# Web based estimator/calculator available






stat %>% 
  ggplot(mapping = aes(x = r.squared)) +
  geom_histogram()

stat %>% 
  ggplot(mapping = aes())
# Take roads --> vmt --> predict --> emissions
predict(m, tibble(vmt = 3))

# What is our goal? ######################
# If I'm a user and I want to know about Bus Emissions in Tompkins County (eg. Yan!)
# Right now, I need to run MOVES --> and it's a process! 
# MOVES - It requires 40 input tables.
# catr - requires just 1 or more
# movesai - requires 1 number.

# Assumptions: ##############################
# -- A1. MOVES **can** validly approximate emissions
# -- A2. A Model of MOVES Default Outputs could still
#    reasonably approximate MOVES outputs from custom input data
# --> Test this assumption (A2)

# How would you test validity?

# E-D = Emissions from MOVES with DEFAULT Input Data
# E-C = Emissions from MOVES with CUSTOM Input Data (or catr)
# E-MD = Predicted Emissions from a model of MOVES with DEFAULT Input Data
# E-MC = Predicted Emissions from a model of MOVES with CUSTOM Input Data

# Z. If E-MD correlates with E-C --> AMAZING

# Goals: ##############################################
# A. Make some functions that can download CAT formatted data from DB
# B. Make some functions that can MODEL that data
# C. Make some kind of checker - is this model good enough?
#    C1. Make a comparer - compare observed to predicted emissions.
# D. Make an example workflow that can actually run Yan's project needs.


dbDisconnect(db); rm(list = ls())
