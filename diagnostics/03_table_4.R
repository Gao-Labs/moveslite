#' @name 03_figure_4.R
#' @author Tim Fraser and Yan Guo (revised from Yan Guo)
#' @description
#' Case Study Analytics

# Load packages
library(dplyr)
library(broom)
library(DBI)
library(RSQLite)
library(ggplot2)

rm(list = ls())
#' These are the 5 core functions in `moveslite`.
# source("R/connect.R")
# source("R/query.R")
# source("R/setx.R")
# source("R/estimate.R")
# source("R/project.R")
# Load the moveslite functions
devtools::load_all(".")

# Connect to case study mini-CATSERVER example
db = dbConnect(drv = RSQLite::SQLite(), "diagnostics/case_study.sqlite")
vars = c("year", "vmt", "vehicles", "sourcehours", "starts")
# Download data (should end up with ~14 rows)
d1 = query(.db = db, .table = "d36109", .filters = list(.pollutant = 98, .by = 8, .sourcetype = 42), .vars = vars)
# Compute the model
m1 = estimate(data = d1, .vars = vars, .best = TRUE)
# Disconnect
dbDisconnect(db); remove(db)


# Equivalent to this:
# formula1 = log(emissions) ~ poly(year,2) + poly(log(vmt), 3) + vehicles + sourcehours + starts
# m = default %>% lm(formula = formula1)


# Let's write a function
# that estimates realistic values for vmt vehicles starts and sourcehours when vmt or vehicles changes.
# Assume certain constants, based on default data or empirics
get_newdata = function(year = 2020, vehicles = 20, vmt_per_vehicle = 43647, mph = 13){
  # Testing values
  # year = 2020
  # vehicles = 45
  # vmt_per_vehicle = 43647
  # mph = 13
  miles_per_hour = mph
  vmt = vmt_per_vehicle * vehicles

  # Distance-adjusted sourcehours measure
  sourcehours_per_vehicle = vmt_per_vehicle / miles_per_hour
  sourcehours = sourcehours_per_vehicle * vehicles

  # Distance- and vehicle- adjusted starts measure
  starts_per_day_per_vehicle = 3.5
  days_per_year = 365.25
  vmt_per_vehicle_per_day = vmt_per_vehicle / days_per_year
  starts_per_day_per_vehicle_per_vmt = starts_per_day_per_vehicle / (vmt_per_vehicle_per_day)
  starts = starts_per_day_per_vehicle_per_vmt * days_per_year * vmt_per_vehicle_per_day * vehicles

  tibble(
    year, vehicles, vmt_per_vehicle, vmt, miles_per_hour,
    sourcehours_per_vehicle, sourcehours,
    starts_per_day_per_vehicle, days_per_year, starts
  )
}


# Test #1
bind_rows(
  get_newdata(year = 2020, vehicles = 45, vmt_per_vehicle = (1527484)/45, mph = 13),
  get_newdata(year = 2025, vehicles = 45, vmt_per_vehicle = (1527484-50000)/45, mph = 13)
) %>%
  select(year, vmt, vehicles, sourcehours, starts) %>%
  project(m = m1, data = d1) %>% filter(type == "custom") %>%
  select(year, vehicles, vmt, sourcehours, starts, emissions)


3.5*365.25

#d1 %>% filter(year == 2020)
v = 45
vmtrate = (1527484)/v

# Test #2
viz = tidyr::expand_grid(
  vehicles = c(v, v+5, v +10),
  year = 2020
) %>%
  mutate(get_newdata(year = year, vehicles = vehicles, vmt_per_vehicle = vmtrate, mph = 13)) %>%
  select(year, vmt, vehicles, sourcehours, starts) %>%
  project(m = m1, data = d1) %>%   filter(type == "custom") %>%
  select(year, vehicles, vmt, sourcehours, starts, emissions) %>%
  arrange(vehicles, year)
viz



viz = tidyr::expand_grid(
  vehicles = c(v, v+5, v +10),
  year = 2020
) %>%
  mutate(get_newdata(year = year, vehicles = vehicles, vmt_per_vehicle = vmtrate, mph = 13)) %>%
  select(year, vmt, vehicles, sourcehours, starts) %>%
  project(m = m1, data = d1) %>%   filter(type == "custom") %>%
  select(year, vehicles, vmt, sourcehours, starts, emissions) %>%
  arrange(vehicles, year)
viz


