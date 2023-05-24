#' @title final 10-15 models
#' @author Yan
#' @describeIn the chosen 10 models to run diagnostic
#'




#######################################################################################

library(dplyr)
library(broom)

#' These are the 5 core functions in `moveslite`.
source("R/connect.R")
source("R/query.R")
source("R/setx.R")
source("R/estimate.R")
source("R/project.R")


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

db %>% dbListTables()
# Download data (should end up with ~14 rows)
default = query(.db = db, .table = .table, .filters = .filters, .vars = .vars)

#1
model = lm(emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours,2) + starts + year
           + year*vmt + year*sourcehours + vmt*sourcehours, data = default)
#2
model = lm(emissions ~  year + vmt + sourcehours + vehicles + starts +
             year*vmt + year*sourcehours + vmt*sourcehours, data = default)

#3
model = lm(emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours,2) + starts + year
           + year*vmt + year*sourcehours + vmt*sourcehours, data = default)

#4
model = lm(emissions ~ poly(vmt, 2) + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year, data = default)

#5
model = lm(emissions ~ poly(vmt, 2) + poly(vehicles, 2) + poly(sourcehours, 2) + starts + year, data = default)

#6
model = lm(emissions ~ poly(vmt, 2) + poly(vehicles,2) + sourcehours + poly(starts, 2) + year, data = default)

#7
model = lm(emissions ~ sqrt(vmt) + sqrt(vehicles) + sqrt(sourcehours) + sqrt(starts) + year, data = default)


formulas <- list(
  formula1  =  emissions ~ poly(vmt, 2) + poly(vehicles,2) + poly(sourcehours, 2) + poly(starts,2) + year,
  formula2  =  emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours, 2) + starts + year,
  formula3  =  emissions ~ year + vmt + sourcehours + vehicles + starts +
               year*vmt + year*sourcehours + vmt*sourcehours,
  formula4  =  emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours,2) + starts + year +
               year*vmt + year*sourcehours + vmt*sourcehours,
  formula5  =  emissions ~ poly(vmt, 2) + poly(vehicles, 2) + poly(sourcehours, 2) + + starts + year,
  formula6  =  emissions ~ poly(vmt, 2) + poly(vehicles,2) + sourcehours + poly(starts, 2) + year,
  formula7  =  emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours, 2) + poly(starts, 2) + year,
  formula8  =  emissions ~ vmt + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year,
  formula9  =  emissions ~ sqrt(vmt) + sqrt(vehicles) + sqrt(sourcehours) + sqrt(starts) + year,
  formula10 =  emissions ~ poly(vmt, 3) + year + vmt*year
)

# Access the formulas
print(formulas)

## need to be adjusted

model %>% glance()
















