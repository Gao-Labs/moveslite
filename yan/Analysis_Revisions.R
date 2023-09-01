#' @title Analysis Revisions
#' @author Yan
#' @description Diagnostic Plot, As x increases from 0 to xxx, what happens to predicted emissions

# Model 22: log(emissions)~year * poly(log(vmt), 3)   - >  Adj.R2=99.3%
# Model 32: log(emissions)~poly(log(vmt), 2) + poly(log(vehicles), 2) + poly(log(sourcehours), 2) + year   → Adj. R2=98.6%
# Model 30: log(emissions)~poly(log(vmt), 2) + poly(log(vehicles), 2) + log(sourcehours) + year  → Adj. R2=0.98

# Model X: log(emissions) ~ log(vmt) + vehicles + sourcehours + year + starts

library(dplyr)
library(broom)
library(DBI)
library(RSQLite)
library(ggplot2)

#' These are the 5 core functions in `moveslite`.
source("R/connect.R")
source("R/query.R")
source("R/setx.R")
source("R/estimate.R")
source("R/project.R")

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

###################################################################################
##############################FORMULA 1 with VMT###################################
###################################################################################

formula1 = log(emissions) ~ poly(log(vmt),3) + (vehicles) + (sourcehours) + poly(year,2) + starts
#formula1 = log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + (sourcehours) + poly(year,2))

# Compute the model
model1 = default %>% lm(formula = formula1)
glance(model1)
summary(model1)

# Create a data frame with the predictor variables
data <- data.frame(emissions = 1338.3,
                   vmt = 761098.4,
                   vehicles = 24.5,
                   sourcehours = 21323.0,
                   year = 2020,
                   starts = 46897.5)

# Create a sequence of vmt values

vmt_seq <- seq(min(default$vmt), max(default$vmt),
               length.out = round(-(min(default$vmt) - max(default$vmt))/100,0))
vmt_seq
data1 <- data.frame(vmt = vmt_seq,
                   vehicles = data$vehicles,
                   sourcehours = data$sourcehours,
                   year = 2020,
                   starts = data$starts)

# Calculate predicted emissions using the model
data1$predicted_emissions <- exp(predict(model1, newdata = data1))
marker_data1 <- data.frame(vmt = 761098.4,
                          emissions = 1338.3)

#
# dat = tibble(
#   year = 2020:2030,
#   vmt = data$vmt,
#   vehicles = data$vehicles,
#   starts = data$starts,
#   sourcehours = data$sourcehours
# ) %>%
#   mutate(vmt = vmt + 100000 * 0:10,
#          vehicles = vmt / 10000,
#          starts = vmt / 20,
#          sourcehours = vmt / 50)
#
#
# dat %>% predict(model1, newdata = .) %>% exp()
#

ggplot(data1, aes(x = vmt, y = predicted_emissions)) +
  geom_line() +
  labs(x = "VMT",
       y = "Predicted Emissions") +
  geom_point(data = marker_data1, aes(x = vmt, y = emissions), color = "red", size = 3, shape = 16) +
  ggtitle("Predicted Emissions vs. VMT") +
  theme_minimal()


###################################################################################
#########################FORMULA 1 with Vehicles###################################
###################################################################################

vehicles_seq <- seq(min(default$vehicles), max(default$vehicles), length.out = -(min(default$vehicles) - max(default$vehicles)))


data2 <- data.frame(vmt = data$vmt,
                    vehicles = vehicles_seq,
                    sourcehours = data$sourcehours,
                    year = 2020,
                    starts = data$starts)

# Calculate predicted emissions using the model
data2$predicted_emissions <- exp(predict(model1, newdata = data2))

marker_data2 <- data.frame(vehicles = 24.5,
                          emissions = 1338.3)

ggplot(data2, aes(x = vehicles, y = predicted_emissions)) +
  geom_line() +
  labs(x = "Vehicles",
       y = "Predicted Emissions") +
  geom_point(data = marker_data2, aes(x = vehicles, y = emissions), color = "red", size = 3, shape = 16) +
  ggtitle("Predicted Emissions vs. Vehicles") +
  theme_minimal()

###################################################################################
########################FORMULA 1 with sourcehours#################################
###################################################################################

sourcehours_seq <- seq(min(default$sourcehours), max(default$sourcehours), length.out = -(min(default$sourcehours) - max(default$sourcehours))/100)


data3 <- data.frame(vmt = data$vmt,
                    vehicles = data$vehicles,
                    sourcehours = sourcehours_seq,
                    year = 2020,
                    starts = data$starts)

# Calculate predicted emissions using the model
data3$predicted_emissions <- exp(predict(model1, newdata = data3))

marker_data3 <- data.frame(sourcehours = 21323.0,
                           emissions = 1338.3)

ggplot(data3, aes(x = sourcehours, y = predicted_emissions)) +
  geom_line() +
  labs(x = "sourcehours",
       y = "Predicted Emissions") +
  geom_point(data = marker_data3, aes(x = sourcehours, y = emissions), color = "red", size = 3, shape = 16) +
  ggtitle("Predicted Emissions vs. sourcehours") +
  theme_minimal()

###################################################################################
########################FORMULA 1 with sourcehours#################################
###################################################################################

sourcehours_seq <- seq(min(default$sourcehours), max(default$sourcehours), length.out = -(min(default$sourcehours) - max(default$sourcehours))/10)


data3 <- data.frame(vmt = data$vmt,
                    vehicles = data$vehicles,
                    sourcehours = sourcehours_seq,
                    year = 2020,
                    starts = data$starts)

# Calculate predicted emissions using the model
data3$predicted_emissions <- exp(predict(model1, newdata = data3))

marker_data3 <- data.frame(sourcehours = 21323.0,
                           emissions = 1338.3)

ggplot(data3, aes(x = sourcehours, y = predicted_emissions)) +
  geom_line() +
  labs(x = "sourcehours",
       y = "Predicted Emissions") +
  geom_point(data = marker_data3, aes(x = sourcehours, y = emissions), color = "red", size = 3, shape = 16) +
  ggtitle("Predicted Emissions vs. sourcehours") +
  theme_minimal()

###################################################################################
########################FORMULA 1 with starts######################################
###################################################################################

starts_seq <- seq(min(default$starts), max(default$starts), length.out = -(min(default$starts) - max(default$starts))/20)


data4 <- data.frame(vmt = data$vmt,
                    vehicles = data$vehicles,
                    sourcehours = data$sourcehours,
                    year = 2020,
                    starts = starts_seq)

# Calculate predicted emissions using the model
data4$predicted_emissions <- exp(predict(model1, newdata = data4))

marker_data4 <- data.frame(starts = 46897.5,
                           emissions = 1338.3)

ggplot(data4, aes(x = starts, y = predicted_emissions)) +
  geom_line() +
  labs(x = "starts",
       y = "Predicted Emissions") +
  geom_point(data = marker_data4, aes(x = starts, y = emissions), color = "red", size = 3, shape = 16) +
  ggtitle("Predicted Emissions vs. starts") +
  theme_minimal()


