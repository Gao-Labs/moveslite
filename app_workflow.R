# app_workflow.R

# Test of core app functions

# Load moveslite working edition
devtools::load_all(".")

# Suppose these testing inputs

input = list(scenario = "granddata.d36109", pollutant = 98, by = "8.41", predictors = c("year", "vmt"), unit = "t", startyear = 2020) # choices_aggregation[[2]][5]
# input = list(scenario = "granddata.dXXXXX", pollutant = 98, by = choices_aggregation[[2]][5])

default = function(){ get_default(.scenario = input$scenario, .pollutant = input$pollutant, .by = input$by) }

# Test it
default()

# Get variables in the data
vars = function(){ get_vars(.default = default()) }

vars() # test it


# Get your model
model = function(){ estimate(data = default(), .vars = input$predictors) }

model() # test it

# Find transformation used
find_transformation(m = model())

# Your newx function (FAKE)
newx = function(){ tibble(year = 2020, vmt = 2276674) }

# Get your estimates
project(m = model(), data = default(), .newx = newx(), .cats = "year", .context = FALSE, .ci = 0.95)
#project(m = model(), data = default(), .newx = newx(), .cats = "year", .context = TRUE, .ci = 0.95)


# Get summary statistics
find_stats(m = model(), .unit = "t")

# Create an 'updated' data table
data = function(){ x = default(); x[7,3] <- 2460000; x[8,3] <- 2480000; x  }

# Identify the rows that changed...
# default()
# data()
newx = function(){ get_newx(.default = default(), .data = data()) }

newx() # test

# Get prediction dataframe
ydata = function(){ get_ydata(.model = model(), .default = default(), .newx = newx()) }

qi = function(){ qi_scenario(.ydata = ydata(), .pollutant = 98, .unit = "t", .startyear = 2020) }

h_scenario(data = qi() )

