#' @name workflow.R
#' @title Example Workflow for `moveslite`
#' @author Tim Fraser
#' @description A demo of the intended workflow for a `moveslite` user.


#' @note Packages you need to load!
library(dplyr)
library(broom)
library(readr)
library(tidyr)
library(stringr)

#' These are the 5 core functions in `moveslite`.
source("R/connect.R")    # connect() to a database
source("R/query.R")      # query() that database in a specific way
source("R/query_aggregate.R")# query_aggregate() - query a special extra subset
source("R/query_many.R")# query_many() - wrapper for making complex queries
source("R/setx.R")       # setx() - create newdata from default data to feed to predict()
source("R/estimate.R")   # estimate() a model of the default data
source("R/project.R")    # generate predictions with project()

#' Our goal is to extend their functionality.

#' Here's an example of their usage.

# Connect to the 'data' database (tenatively your z/db.sqlite file)
db = connect("data")

# Total CAT Format is about n = 9000
# by = 16 = Overall
# by = 8 = Sourcetype
# by = 12 = Regulatory Class
# by = 14 = Fueltype
# by = 15 = Roadtype


query_many(db, .table = "d36109", .filters = list(.pollutant = 98, .by = 8),
           .vars = c("sourcetype", "emissions"))

query_many(db, .table = "d36109", .filters = list(.pollutant = 98, .by = 8, .sourcetype = 31),
           .vars = c("sourcetype", "emissions"))

query_many(db, .table = "d36109", .filters = list(.pollutant = 98, .by = 8, .sourcetype = c(21,31)),
           .vars = c("sourcetype", "emissions"))

query_many(db, .table = "d36109", .filters = list(.pollutant = 98, .by = 17),
           .vars = c("emissions", "vehicles"))


query_many(db, .table = "d36109", .filters = list(.pollutant = 98, .by = 20),
           .vars = c("emissions", "vmt"))

query_many(db, .table = "d36109", .filters = list(.pollutant = 98, .by = c(16, 8)),
           .vars = c("emissions", "vehicles", "vmt"))

# Simplify categories


data %>%
  lm(formula = emissions ~ year + poly(vmt, 2) + bus + car_bike + combo_truck - 1) %>%
  summary()

dbDisconnect(db); rm(list = ls()); gc()
# Idea:

# GEOID

# PROCESS: MOVESLiter vs. MOVESHeavier

# AGGREGATION

# TYPE

# SUBTYPE

# MODEL: 1, 2, or 3














