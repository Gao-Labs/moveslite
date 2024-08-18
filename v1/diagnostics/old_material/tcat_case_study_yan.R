#' @title tcat case study
#' @author Yan
#' @describeIn a case study using MOVES Lite


library(readxl)
X2020_average <- read_excel("C:/Users/Yan/Desktop/2020_average.xls")

library(dplyr)

# change column name
colnames(X2020_average) <- c("Day", "Route", "Year", "Date", "Average_mile", "Average_minutes", "Daily_Trips", "MPH", "Route Number")

# subset information that are needed
trip_info_2020 <- X2020_average[c("Day", "Route", "Year", "Average_mile", "Average_minutes", "Daily_Trips", "Date")]

# change minutes to hours, calculate total hours and miles by using average * number
trip_info_2020 <- trip_info_2020 %>%
  mutate(total_hours = (Average_minutes * Daily_Trips)/60,
         total_miles = Average_mile * Daily_Trips)

# calculate the average
route_info <- trip_info_2020 %>%
  group_by(Day, Route) %>%
  summarize(total_hours = mean(total_hours),
            total_miles = mean(total_miles))

# calculate the information of each day
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

# Use unique() to get the unique numbers
unique_numbers <- unique(trip_info_2020$Route)

# Count the unique numbers using length()
count <- length(unique_numbers)

library(dplyr)
library(broom)

#' These are the 5 core functions in `moveslite`.
source("R/connect.R")
source("R/query.R")
source("R/setx.R")
source("R/estimate.R")
source("R/project.R")

#' Here's an example of their usage.
devtools::load_all(".")

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

# calculate the total vmt, and total sourcehours
.vmt = total_mile[1]*262 + total_mile[2]*52 + total_mile[3]*52
.sourcehours = total_hour[1]*262 + total_hour[2]*52 + total_hour[3]*52

dbDisconnect(db); remove(db)

tcat_2020 <- data.frame(
  year = 2020,
  vmt = total_mile[1]*262 + total_mile[2]*52 + total_mile[3]*52,
  sourcehours = total_hour[1]*262 + total_hour[2]*52 + total_hour[3]*52,
  vehicles = 45,
  start = 180000
)

# Estimate the model
############################################################################################
model = default %>% lm(formula = log(emissions) ~ poly(log(vmt),3) + (vehicles) + (sourcehours) + poly(year,2) + starts)

model %>% glance()

# as vmt increase by 500000, how does the emission changes
# for prediction of other others, just change the number in the project function
output = project(m = model, data = default,
        .newx = tibble(year = 2020, vmt = 1527484, vehicles = 45, sourcehours = 100866.5, starts = 58500),
        .context = FALSE, .exclude = "geoid", .cats= "year")


# Source


tribble(
  ~value,   ~units,           ~name,                                            ~source,
  "43,647", "miles per year", "average annual mileage for public transit buses", "DOT 2019" ,
  "20",     "vehicles",       "average buses per county",                        "Assumption",
  "13",     "miles per hour",  "average speed of public buses in metro areas",   "NYC DOT 2018",
  "3,357", "hours per vehicle", "average rate of sourcehours per vehicle",      "Derived",
  "3~4 (3.5)",   "routes",            "average bus routes completed per day",         "Assumption",
  "1,300",    "starts per vehicle per year", "average starts per bus per year",          "Derived",
  # Empirical
  "45",      "vehicles",      "TCAT Buses in Tompkins County",                    "TCAT 2020",
  "1,527,484", "vehicle miles traveled", "2020 empirical vehicle miles traveled",           "Derived",
  "100,866.5",  "hours",        "2020 empirical sourcehours",                      "Derived"
)

# Estimating Emissions for a Policy Decreasing Vehicle Miles Traveled
bind_rows(
  tibble(year = 2020, vmt = 1527484, vehicles = 45, sourcehours = 100866.5, starts = 58500),
  tibble(year = 2020, vmt = 1527484 - 50000, vehicles = 45, sourcehours = 100866.5, starts = 58500)
) %>%
  project(m = model, data = default, .newx = ., .context = FALSE, .exclude = "geoid", .cats= "year") %>%
  filter(type == "custom")

# 2,898.76 tons to 2,329.62

# Estimating Emissions for a Policy Increasing Vehicle Availability
bind_rows(
  tibble(year = 2020, vmt = 1527484, vehicles = 45, sourcehours = 100866.5, starts = 58500),
  tibble(year = 2020, vmt = 1527484 + 50000, vehicles = 45, sourcehours = 100866.5, starts = 58500)
) %>%
  project(m = model, data = default, .newx = ., .context = FALSE, .exclude = "geoid", .cats= "year") %>%
  filter(type == "custom")


# 43647
# 20
# 13 miles



library(ggplot2)
library(scales)

# Create a data frame with the emissions data
data <- data.frame(
  x = c("Bus", "Bus", "Car", "Car"),
  y = c("Current", "Predicted", "Current", "Predicted"),
  Emission = c(2889, 3607, 94432, 95946)
)

# Create a side-by-side bar plot with different scales
plot <- ggplot(data, aes(x = x, y = Emission, fill = y)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  labs(title = "Emission Comparison for Buses and Cars",
       x = "Vehicle",
       y = "Emission",
       fill = "Emission Type") +
  theme_minimal() +
  scale_y_continuous(labels = comma_format(scale = 1e-3), breaks = seq(0, 100, by = 20)) +
  scale_fill_manual(values = c("Current" = "blue", "Predicted" = "red")) +
  facet_wrap(~x, scales = "free_y")

# Add numbers on top of the bars
plot <- plot + geom_text(aes(label = Emission), position = position_dodge(width = 0.9), vjust = -0.5)
