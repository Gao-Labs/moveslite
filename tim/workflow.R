#' @name workflow.R
#'
#' @description A demo of the intended workflow for a `moveslite` user.


# - roxygen?
# - devtools
# - usethis
# - function files


library(dplyr)
library(broom)

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

# Here's the geoid for tompkins county, as a named vector
.geoid = c("Tompkins County" = "36109")
.table = paste0("d", .geoid)
# Here's the type of data we want, named for your convenience
.pollutant = c("CO2e" = 98)
.by = c("Overall" = 16)
# Make filters and list variables
.filters = c(.pollutant = unname(.pollutant), .by = unname(.by) )
.vars = c("year", "vmt", "vehicles", "sourcehours", "starts")

# Download data (should end up with ~14 rows)
default = query(.db = db, .table = .table, .filters = .filters, .vars = .vars)

# Disconnect
dbDisconnect(db); remove(db)


# Estimate the model
model = estimate(data = default, .vars = .vars)

# View its quality of fit
model %>% glance()

# Suppose we had some information about 1 or more variables for a custom scenario year
.newx = list(year = 2023, vmt = 343926)

# Quantities of interest
qis = project(m = model, data = default, .newx = .newx,
        .cats = "year", .exclude = "geoid", .context = TRUE)

# Look at the custom prediction versus the benchmark
qis %>% filter(type %in% c("custom", "benchmark"))

qis %>% filter(type %in% c("custom", "benchmark")) %>%
  ggplot(mapping = aes(x = year, y = emissions, color = type, group = 1)) +
    geom_point(mapping = aes(color = type)) +
    geom_line()

# Look at the benchmark years
qis %>% filter(type %in% c("pre_benchmark", "benchmark", "post_benchmark"))

qis %>% filter(type %in% c("pre_benchmark", "benchmark", "post_benchmark"))  %>%
  ggplot(mapping = aes(x = year, y = emissions, color = type, group = 1)) +
  geom_point(mapping = aes(color = type)) +
  geom_line()
