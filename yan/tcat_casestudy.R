#' @title tcat case study
#' @author Yan
#' @describeIn a case study using MOVES Lite


library(readxl)
X2020_average <- read_excel("C:/Users/Yan/Desktop/2020_average.xls")

library(dplyr)

colnames(X2020_average) <- c("Day", "Route", "Year", "Date", "Average_mile", "Average_minutes", "Daily_Trips", "MPH", "Route Number")

trip_info_2020 <- X2020_average[c("Day", "Route", "Year", "Average_mile", "Average_minutes", "Daily_Trips", "Date")]

trip_info_2020 <- trip_info_2020 %>%
  mutate(total_hours = (Average_minutes * Daily_Trips)/60,
         total_miles = Average_mile * Daily_Trips)

route_info <- trip_info_2020 %>%
  group_by(Day, Route) %>%
  summarize(total_hours = mean(total_hours),
          total_miles = mean(total_miles))

days <- c("Wkdy", "Sun", "Sat")

# Initialize an empty vector to store the total miles for each day
total_mile <- c()
total_hour <- c()

# Loop through each day and calculate the sum of miles
for (day in days) {

  # Subset the data for the current day
  subset_data <- route_info[route_info$Day == day, ]

  # Calculate the sum of miles + hours for the current day
  sum_miles <- sum(subset_data$total_miles)
  sum_hours <- sum(subset_data$total_hours)

  # Store the sum of miles + hours in the total_miles vector
  total_mile <- c(total_mile, sum_miles)
  total_hour <- c(total_hour, sum_hours)
}


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
.by = c("Overall" = 8)
# Make filters and list variables
.filters = c(.pollutant = unname(.pollutant), .by = unname(.by) )
.vars = c("year", "vmt", "vehicles", "sourcehours", "starts", "sourcetype")

# Download data (should end up with ~14 rows)
default = query(.db = db, .table = .table, .filters = .filters, .vars = .vars)

default <- default %>%
  filter((sourcetype %in% c(42)))

default <- default %>%
  group_by(year) %>%
  summarize_if(is.double, sum)

vmt = total_mile[1]*262 + total_mile[2]*52 + total_mile[3]*52
sourcehours = total_hour[1]*262 + total_hour[2]*52 + total_hour[3]*52

dbDisconnect(db); remove(db)

tcat_2020 <- data.frame(
  year = 2020,
  vmt = total_mile[1]*262 + total_mile[2]*52 + total_mile[3]*52,
  sourcehours = total_hour[1]*262 + total_hour[2]*52 + total_hour[3]*52,
  vehicles = 45,
  start = 180000
)

# Estimate the model
#############################################################################################
model = estimate(data = default, .vars = .vars)

summary(model)
# View its quality of fit
model %>% glance()

# Suppose we had some information about 1 or more variables for a custom scenario year
.newx1 = list(vmt = 700000)

.newx2 = list(year = 2020,
             vmt = 3037774,
             sourcehours = 85219.7,
             vehicles = 102.2,
             starts = 175813.0)

# Quantities of interest
qis = project(m = model, data = default, .newx = .newx1,
              .cats = "year", .exclude = "geoid", .context = TRUE)

# Look at the custom prediction versus the benchmark
qis %>% filter(type %in% c("custom", "benchmark"))
