#' @title tcat case study
#' @author Yan, with Tim's edits
#' @describeIn a case study using MOVES Lite

library(dplyr)
library(broom)
library(DBI)
library(RSQLite)

#' These are the 5 core functions in `moveslite`.
source("R/connect.R")
source("R/query.R")
source("R/setx.R")
source("R/estimate.R")
source("R/project.R")

#' Our goal is to extend their functionality.

#' Here's an example of their usage.

# Connect to the 'data' database (tenatively your z/db.sqlite file)
db = connect("data")

vars = c("year", "vmt", "vehicles", "sourcehours", "starts")

# Download data (should end up with ~14 rows)
default = query(
  .db = db,
  .table = "d36109",
  .filters = c(.pollutant = 98, .by = 8, .sourcetype = 42),
  .vars = vars)

dbDisconnect(db); remove(db)

# Estimate the model
model = estimate(data = default, .vars = vars)

# View its quality of fit
model %>% glance()

# Projection
project(
  m = model, data = default,
  # FIX 1: YEAR WAS MISSING IN .newx --> always needs a year. #############################
  # No matter what, you always must supply a year - because that's necessary for it to guess the other values.
  .newx = list(year = 2020),
  .cats = "year", .exclude = "geoid", .context = FALSE)


# FIX 2: #############################
# This framework works!!!
default_vmt = 761098

project(
  m = model, data = default,
  # FIX 1: YEAR WAS MISSING IN .newx --> always needs a year. #############################
  # No matter what, you always must supply a year - because that's necessary for it to guess the other values.
  .newx = list(year = 2020, vmt = default_vmt * 0.99999),
  .cats = "year", .exclude = "geoid",
  # Exclude the extra contextual observations
  .context = FALSE)

# FIX 3: ###############################

# Bad news: The model is HYPER sensitive around VMT, and is able to predict negative emissions. I don't like that.
# So, we probably want to add some log-transformations the outcome (and maybe predictors) like so:
model = default %>% lm(formula = log(emissions) ~ poly(vmt, 2) + year)
glance(model)
# This works much more naturally.
project(
  .newx = list(year = 2020, vmt = default_vmt * 0.50),
  m = model, data = default, .cats = "year", .exclude = "geoid", .context = FALSE)
