#' @name workflow
#' @author Tim Fraser & Yan Guo
#' @description Remind us how to use the package! Demonstrate an example workflow!

#' @note Packages you need to load!

library(dplyr)
library(broom)

#' These are the core functions in `moveslite`.
# source("R/query.R")      # query() that database in a specific way
# source("R/setx.R")       # setx() - create newdata from default data to feed to predict()
# source("R/estimate.R")   # estimate() a model of the default data
# source("R/project.R")    # generate predictions with project()

# Let's load this package as a development version...
# devtools::load_all()

# Let's install it from source!
# Install package from source
install.packages("moveslite_2.0.1.tar.gz", type = "source")

# MOVES cheatsheet, can find information about sourcetype, roadtype, fueltype or emissiontype
# https://github.com/USEPA/EPA_MOVES_Model/blob/master/docs/MOVES4CheatsheetOnroad.pdf
library(moveslite)

#' Here's an example of their usage.

# https://www2.census.gov/programs-surveys/decennial/2010/partners/pdf/FIPS_StateCounty_Code.pdf
# full list can be found in the link

# Total CAT Format is about n = 9000
# by = 16 = Overall
# by = 8 = Sourcetype
# by = 12 = Regulatory Class
# by = 14 = Fueltype
# by = 15 = Roadtype

# with by = sourcetype, and pollutant = carbon dioxide, and county = Tompkin, NY

# Check status of API - good way to warm it up
check_status()

# Let's try a few test queries

# CO2 equivalent emissions overall for geoid 36109
query(geoid = "36109", pollutant = 98, aggregation = 16, var = c("vmt", "vehicles"))

# CO2 equivalent emissions by sourcetype
query(geoid = "36109", pollutant = 98, aggregation = 8, var = c("vmt", "vehicles"))

# CO2 equivalent emissions for a specific pair of sourcetypes
query(geoid = "36109", pollutant = 98, aggregation = 8, sourcetype = c(21, 31), var = c("vmt", "vehicles"))



# Here's the geoid for tompkins county, as a named vector
.geoid = c("Tompkins County" = "36109")
.table = paste0("d", .geoid)

# Here's the type of data we want, named for your convenience
.pollutant = c("CO2e" = 98)
.by = c("Sourcetype" = 8)
.sourcetype = c("Public Transit" = 42) # sourcetype = 42 = public transit, this information can be found in MOVES cheatsheet

# Make filters and list variables
.filters = list(.pollutant = unname(.pollutant), .by = unname(.by) , .sourcetype = unname(.sourcetype))
.vars = c("year", "vmt", "vehicles", "sourcehours", "starts")

# Download data (should end up with ~14 rows)
default = query(geoid = .geoid, pollutant = .filters$.pollutant, aggregation = .filters$.by,
      sourcetype = .filters$.sourcetype, var = .vars)

# View it
default

# Estimate the model
model = estimate(data = default, .vars = .vars)

# View its quality of fit
model %>% glance()


# Suppose we had some information about 1 or more variables for a custom scenario year
# You can change "year", "vmt", "vehicles", "sourcehours", "starts" as you want
.newx = list(year = 2023, vmt = 343926)
.newx = tibble(year = 2023, vmt = 343926)
.newx = tibble(year = c(2023:2024), vmt = c(23023023, 234023402))

# Quantities of interest
qis = project(m = model, data = default, .newx = .newx, .context = FALSE)

# Look at the custom prediction versus the benchmark
qis %>% filter(type %in% c("custom", "benchmark"))

library(ggplot2)
qis %>%
  filter(type %in% c("custom", "benchmark")) %>%
  ggplot(mapping = aes(x = year, y = emissions, color = type, group = 1)) +
  geom_point(mapping = aes(color = type)) +
  geom_line()

# Look at the benchmark years
qis %>% filter(type %in% c("pre_benchmark", "benchmark", "post_benchmark"))

qis %>% filter(type %in% c("pre_benchmark", "benchmark", "post_benchmark"))  %>%
  ggplot(mapping = aes(x = year, y = emissions, color = type, group = 1)) +
  geom_point(mapping = aes(color = type)) +
  geom_line()

rm(list = ls())
