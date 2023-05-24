#' @name model_selection.R
#' @author Yan Guo
#'
#' @description Find models that have r2 0.999



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


# with by = 8

# Correlation Check
cor(default[, -1])

library(ggplot2)
# plot emission
ggplot(data = default, aes(x = year, y = emissions)) +
  geom_point() +
  labs(title = "Scatter Plot of Emissions")




# Create a new plot window
par(mfrow = c(2, 3))  # Adjust the layout based on the number of variables

# Iterate over each variable and create scatter plots
for (variable in c("year", "vmt", "vehicles", "sourcehours", "starts")) {
  plot(emissions ~ get(variable), data = default, main = paste("Scatter plot:", variable))
}

# Reset the plot window layout
par(mfrow = c(1, 1))

# without interaction terms
model = lm(emissions ~ vmt + starts + vehicles + sourcehours + year, data = default)
# r2 = 0.9143

# vehicles is not significant
model = lm(emissions ~ vmt + starts + sourcehours + year, data = default[, -1])
# r2 = 0.9136

# add poly,2 to vmt, vehicles, sourcehours, starts

model = lm(emissions ~ poly(vmt, 2) + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year -1, data = default[, -1])
# r2 = 0.9857

#drop poly for starts
model = lm(emissions ~ poly(vmt, 2) + poly(vehicles, 2) + poly(sourcehours, 2) + starts + year -1, data = default[, -1])
# r2 = 0.9851

# add poly, 3 to vmt, vehicles, sourcehours, starts

model = lm(emissions ~ poly(vmt, 3) + poly(vehicles, 3) + poly(sourcehours, 3) + poly(starts, 3) + year -1, data = default[, -1])
# r2 = 0.9919


# possible interaction terms

# 2 terms
# year*vmt, year*vehicles, year*sourcehours, year*starts
# vmt*vehicles, vmt*sourcehours, vmt*starts
# vehicle*sourcehours, vehicle*starts

model = lm(emissions ~ .^2-1, data = default[, -1])
# with intercept term - r2 = 0.9955
# without intercept term - r2 = 0.9968

# drop interaction terms that p values are greater than 0.05
# keep year:vmt, year:sourcehours, year:starts, vmt:sourcehours, vehicles:starts
# re-run the model

model = lm(emissions ~ year + vmt + vehicles + sourcehours + starts +
                       year*vmt + year*vehicles + year*sourcehours + year*starts + vmt*sourcehours + vehicles*starts -1,
                       data = default[, -1])
#r2 = 0.9865

# some parameters become insignificant, drop year:starts

model = lm(emissions ~ year + vmt + vehicles + sourcehours + starts +
                       year*vmt + year*vehicles + year*sourcehours + vmt*sourcehours + vehicles*starts -1,
                       data = default[, -1])

#r2 = 0.9864

# 3 terms
# year*vmt*vehicles, year*vmt*sourcehours, year*vmt*starts
# vmt*vehicles*sourcehours, vmt*vehicles*starts
# vmt*sourcehours*starts


model = lm(emissions ~ .^3, data = default[, -1])
# with intercept r2 = 0.9991
# without intercept r2 = 0.9988

# drop vmt:vehicles:starts

model = lm(emissions ~ .^2 + year*vmt*vehicles + year*vmt*sourcehours +
                       year*vmt*starts + vmt*vehicles*sourcehours +
                       vmt*sourcehours*starts -1, data = default[, -1])
# r2 = 0.9982

# drop year:sourcehours, year:starts,

model = lm(emissions ~ year + vmt + vehicles + sourcehours + starts +
             year*vmt + year*vehicles + vmt*vehicles + vmt*sourcehours + vmt*starts +
             vehicles*sourcehours + vehicles*starts + sourcehours*starts +
             year*vmt*vehicles +
             year*vmt*starts + vmt*vehicles*sourcehours +
             vmt*sourcehours*starts -1, data = default[, -1])
# r2 = 0.9971

#drop all other 3 terms

model = lm(emissions ~ year + vmt + vehicles + sourcehours + starts +
             year*vmt + year*vehicles + vmt*sourcehours +
             vehicles*sourcehours + vehicles*starts + sourcehours*starts -1, data = default[, -1])
# r2 = 0.9917


model = lm(emissions ~ year + vmt + vehicles + sourcehours +
             year*vehicles + vmt*sourcehours -1, data = default[, -1])
# r2 = 0.9824

summary(model)


# 4 terms
#year*vmt*vehicles*sourcehours, year*vmt*vehicles*starts
#vmt*vehicle*sourcehours*starts


model = lm(emissions ~ .^4, data = default[, -1])
# r2 = 0.9995


#5 terms
#year*vmt*vehicles*sourcehours*starts
# no need to do 5 terms


# 14 observations data

model = lm(emissions ~ poly(vmt, 2) + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year-1, data = default[, -1])
# r2 = 0.9999
summary(model)
car::vif(model)

model = lm(emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours, 2) + starts + year -1, data = default[, -1])
# r2 = 0.9997

model = lm(emissions ~ vmt + vehicles + sourcehours + starts + year -1, data = default[, -1])
# r2 = 0.9968

summary(model)


# Apply the interaction terms

model = lm(emissions ~  year + vmt + sourcehours + vehicles + starts +
           year*vmt + year*sourcehours + vmt*sourcehours -1, data = default[, -1])
# r2 = 1
# vehicles + starts can be droped

# Apply the interaction terms + poly

model = lm(emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours,2)
                       + year*vmt + year*sourcehours + vmt*sourcehours -1, data = default[, -1])
# r2 = 1


# Apply the square root terms + poly


model = lm(emissions ~ sqrt(vmt) + vehicles + sqrt(sourcehours) + sqrt(starts) + year -1, data = default[, -1])
# r2 = 0.9983

summary(model)

# test the model

test = default %>%
  filter (fueltype == 1)

model = lm(emissions ~ poly(vmt, 2) + vehicles + poly(sourcehours, 2) +
             starts + year -1, data = test[, -1])
# r2 = 0.999

model = lm(emissions ~  year + vmt + sourcehours + vehicles + starts +
             year*vmt + year*sourcehours + vmt*sourcehours -1, data = test[, -1])
#r2 = 0.9999
library(dplyr)
library(readr)
library(purrr)
library(broom)
model %>% glance()

#############################################


.data = test


diagnostic(test, .formula = emissions ~ year + poly(vmt, 3))

rm(list = ls())

tabler = function(.geoid = "36109", .pollutant = 98, .by = 8, .type = 42){
  library(dplyr)
  library(readr)
  library(purrr)
  library(broom)
  source("R/connect.R")

  # For any formula, evaluate it.
  diagnostic = function(.data, .formula){

    library(dplyr)
    library(broom)

    model = lm(formula = .formula, data = .data)
    output = model %>%
      glance() %>%
      mutate(formula = as.character(.formula)[3]) %>%
      select(adjr = adj.r.squared, sigma, df.residual, formula)
    return(output)

  }


  db = connect("data")

  data = db %>%
    tbl(paste0("d", .geoid)) %>%
    filter(pollutant == !!.pollutant & by == !!.by) %>%
    collect()
  dbDisconnect(db)

  # Only if .by is NOT overall
  if(.by != 16){
    .byvar = case_when(
      .by == 8 ~ "sourcetype",
      .by == 14 ~ "fueltype",
      .by == 12 ~ "regclass",
      .by == 15 ~ "roadtype")
    data = data %>%
      filter(!!sym(.byvar) == .type)
  }

  tab = list(
    emissions ~ year + poly(vmt, 3),
    emissions ~ year + poly(vmt, 2),
    emissions ~ year + poly(vmt, 1)
  ) %>%
    # Apply this function many times
    map(.f = ~diagnostic(.data = data,  .formula = .x), .id = "id") %>%
    # Bind a list of data.frames together
    bind_rows(.id = "id") %>%
    arrange(desc(adjr))

  tab = tab %>%
    mutate(geoid = .geoid,
           pollutant = .pollutant,
           by = .by,
           # If you have a type, fill it in; otherwise, make it blank
           type = if_else(.by != 16, true = .type, false = NA_integer_))
  return(tab)
}

tab = tabler(.geoid = "36109", .pollutant = 98, .by = 8, .type = 42)

tab %>%
  write_csv("yan/table_a1.csv")

source("R/tabler.R")

source("R/connect.R")
db = connect("data")
# Get just the full vector of table names that are counties (5 digits)
alltables = tibble(tables = db %>% dbListTables()) %>%
  filter(stringr::str_detect(tables, pattern = "d[0-9]{5}")) %>%
  with(tables)

allgeoids = stringr::str_remove(alltables, "d")
allgeoids
##########################################################################################
##########################################################################################
##########################################################################################

set.seed(123)  # Set a random seed for reproducibility
train_size <- floor(0.7 * nrow(default))
train_indices <- sample(seq_len(nrow(default)), size = train_size)

train_data <- default[train_indices, ]
test_data <- default[-train_indices, ]

# Train the linear regression model
model = lm(emissions ~  year + vmt + sourcehours + vehicles + starts +
             year*vmt + year*sourcehours + vmt*sourcehours -1, data = train_data[, -1])
# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Calculate evaluation metrics
mse <- mean((test_data[["emissions"]] - predictions)^2)
rmse <- sqrt(mse)
r_squared <- summary(model)$r.squared



# Disconnect
dbDisconnect(db); remove(db)










