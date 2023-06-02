#' @name workflow.R
#' @title Example Workflow for `moveslite`
#' @author Tim Fraser
#' @description A demo of the intended workflow for a `moveslite` user.


#' @note Packages you need to load!
library(dplyr)
library(broom)

#' These are the 5 core functions in `moveslite`.
source("R/connect.R")    # connect() to a database
source("R/query.R")      # query() that database in a specific way
source("R/setx.R")       # setx() - create newdata from default data to feed to predict()
source("R/estimate.R")   # estimate() a model of the default data
source("R/project.R")    # generate predictions with project()

#' Our goal is to extend their functionality.

#' Here's an example of their usage.

# Connect to the 'data' database (tenatively your z/db.sqlite file)
db = connect("data")


db %>% dbListTables()

# Total CAT Format is about n = 9000
# by = 16 = Overall
# by = 8 = Sourcetype
# by = 12 = Regulatory Class
# by = 14 = Fueltype
# by = 15 = Roadtype


d = db %>%
  tbl("d36109") %>%
  filter(by == 8, pollutant == 98) %>%
  select(year, geoid, emissions, vehicles) %>%
  collect()


d









db %>%
  tbl("d36109") %>%
  count()

db %>%
  tbl("d36109") %>%
  select(by) %>%
  distinct()


db %>%
  tbl("d36109") %>%
  filter(by == 15 & pollutant == 98) %>%
  filter(roadtype == 2) %>%
  count()


db %>%
  tbl("d36109") %>%
  filter(pollutant == 98 & by == 8 & sourcetype == 41) %>%
  glimpse()

dat = db %>%
  tbl("d36109") %>%
  filter(pollutant == 98 & by == 8 & sourcetype == 41) %>%
  collect()

dat %>%
  lm(formula = emissions ~ vmt + year + vehicles + sourcehours + starts) %>%
  glance()

dat %>%
  lm(formula = emissions ~ vmt * year + vehicles + sourcehours + starts) %>%
  glance()

dat %>%
  lm(formula = emissions ~ poly(vmt, 3) + year + vehicles + sourcehours + starts) %>%
  glance()


dat %>%
  lm(formula = emissions ~ poly(vmt, 3) * year + vehicles + sourcehours + starts) %>%
  glance()


# Here's the geoid for tompkins county, as a named vector
.geoid = c("36109")
.table = paste0("d", .geoid)

# Here's the type of data we want, named for your convenience
.pollutant = c( 98)
.by = c( 16)
# Make filters and list variables
.filters = c(.pollutant = unname(.pollutant), .by = unname(.by) )

.vars = c("year", "vmt", "vehicles", "sourcehours", "starts")

# Download data (should end up with ~14 rows)
default = query(.db = db, .table = .table, .filters = .filters, .vars = .vars)


# Estimate the model
model = estimate(data = default, .vars = .vars)


# View its quality of fit
model %>% glance()




# Suppose we had some information about 1 or more variables for a custom scenario year
.newx = list(year = 2023, vmt = 343926)
.newx = tibble(year = 2023, vmt = 343926)
.newx = tibble(year = c(2023:2024), vmt = c(23023023, 234023402))


# Quantities of interest
qis = project(
  m = model, data = default, .newx = .newx, .context = TRUE,
  .cats = "year", .exclude = "geoid")

qis = project(m = model, data = default, .newx = .newx, .context = FALSE)


# Look at the custom prediction versus the benchmark
qis %>% filter(type %in% c("custom", "benchmark"))

library(ggplot2)
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



# Disconnect
dbDisconnect(db); remove(db)
