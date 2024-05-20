#' @name 05_tcat_analysis
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

# Connect to case study mini-CATSERVER example
db = dbConnect(drv = RSQLite::SQLite(), "diagnostics/case_study.sqlite")

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

library(dplyr)

# Let's write a function
# that estimates realistic values for vmt vehicles statrs and sourcehours when vmt or vehicles changes.
new = function(.year = 2020, .vmt =43647,  .vehicles = 10, .mph = 13){
  dplyr::tribble(
    ~year,  ~vmt,                ~vehicles,   ~sourcehours,       ~starts,
    .year,   .vmt * .vehicles,   .vehicles,   .vehicles*.vmt/.mph,  2000*.vehicles
  )
}


# Create 3 datasets of newdata for predictors,
# showing hypothetical changes in activity predictors over time.
# We'll feed these to our model to get emissions estimates.
# Scenario 1
d1 = bind_rows(
  new(.year = 2020, .vehicles = 10),
  new(.year = 2025, .vehicles = 15),
  new(.year = 2030, .vehicles = 20),
  new(.year = 2040, .vehicles = 25),
  new(.year = 2050, .vehicles = 30)
)
# Scenario 2
d2 = bind_rows(
  new(.year = 2020, .vehicles = 10),
  new(.year = 2025, .vehicles = 9),
  new(.year = 2030, .vehicles = 8),
  new(.year = 2040, .vehicles = 7),
  new(.year = 2050, .vehicles = 6)
)
# Scenario 3
myvmt = 43647
d3 = bind_rows(
  new(.year = 2020, .vmt = myvmt),
  new(.year = 2025, .vmt = myvmt - 1000),
  new(.year = 2030, .vmt = myvmt - 2000),
  new(.year = 2040, .vmt = myvmt - 3000),
  new(.year = 2050, .vmt = myvmt - 4000)
)

# Write our formula
formula1 = log(emissions) ~ poly(log(vmt), 3) + (vehicles) + sqrt(sourcehours) + poly(year,2) + starts
#formula1 = log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + (sourcehours) + poly(year,2))

# Compute the model
model1 = default %>% lm(formula = formula1)
#glance(model1)
summary(model1)

# Create a data frame with the predictor variables
data <- data.frame(
  emissions = 1338.3,
  vmt = 761098.4,
  vehicles = 24.5,
  sourcehours = 21323.0,
  year = 2020,
  starts = 46897.5)

# Create a sequence of vmt values,
# spanning the range of the original training data, called 'default'
span = default %>%
  summarize(
    from = min(vmt),
    to = max(vmt),
    n = round(- (min(vmt) - max(vmt)) / 100, 0)
  )

data1 = span %>%
  # Vary vmt
  reframe(vmt = seq(from = from, to = to, length.out = n)) %>%
  # Bind in the constant values
  bind_cols(data %>% select(-vmt) ) %>%
  # Calculate predicted emissions using the model
  mutate(predicted_emissions = exp(predict(model1, newdata = .)) )

marker_data1 <- data.frame(vmt = 761098.4, emissions = 1338.3)


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


data4 <- data.frame(vmt = 1527484+50000,
                    vehicles = 45,
                    sourcehours = 100866.5,
                    year = 2020,
                    starts = 45*1300)

# Calculate predicted emissions using the model
data4$predicted_emissions <- exp(predict(model1, newdata = data4))


data5 <- data.frame(vmt = 278947275-2000000,
                    vehicles = 25043.6,
                    sourcehours = 7975749,
                    year = 2020,
                    starts = 29612701)

f = log(emissions) ~ poly(log(vmt),3) + (vehicles) + (sourcehours) + poly(year,2) + starts
#f = log(emissions) ~ poly(log(vmt),2) + sqrt(vehicles) + sqrt(sourcehours) + year
#formula1 = log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + (sourcehours) + poly(year,2))

# Compute the model
m = default %>% lm(formula = f)

glance(m)

# Calculate predicted emissions using the model
data5$predicted_emissions <- exp(predict(m, newdata = data5))


marker_data4 <- data.frame(starts = 46897.5,
                           emissions = 1338.3)

ggplot(data4, aes(x = starts, y = predicted_emissions)) +
  geom_line() +
  labs(x = "starts",
       y = "Predicted Emissions") +
  geom_point(data = marker_data4, aes(x = starts, y = emissions), color = "red", size = 3, shape = 16) +
  ggtitle("Predicted Emissions vs. starts") +
  theme_minimal()





############################################################################



# Let's write a function
# that estimates realistic values for vmt vehicles statrs and sourcehours when vmt or vehicles changes.
new = function(.year = 2020, .vmt =43647,  .vehicles = 10, .mph = 13){
  tribble(
    ~year,  ~vmt,                ~vehicles,   ~sourcehours,       ~starts,

    .year,   .vmt * .vehicles,   .vehicles,   .vehicles*.vmt/.mph,  1300*.vehicles

  )
}

d1 = bind_rows(
  new(.year = 2020, .vehicles = 20),
  new(.year = 2025, .vehicles = 30),
  new(.year = 2030, .vehicles = 40),
  new(.year = 2040, .vehicles = 60),
  new(.year = 2050, .vehicles = 80)
)

d2 = bind_rows(
  new(.year = 2020, .vehicles = 40),
  new(.year = 2025, .vehicles = 30),
  new(.year = 2030, .vehicles = 20),
  new(.year = 2040, .vehicles = 15),
  new(.year = 2050, .vehicles = 10)
)

myvmt = 43647
d3 = bind_rows(
  new(.year = 2020, .vmt = myvmt),
  new(.year = 2025, .vmt = myvmt - 1000),
  new(.year = 2030, .vmt = myvmt - 2000),
  new(.year = 2040, .vmt = myvmt - 3000),
  new(.year = 2050, .vmt = myvmt - 4000)
)



output = bind_rows(d1,d2,d3, .id = "id") %>%
  mutate(emissions = predict(m, newdata = .) %>% exp())

g1 = ggplot(data = output %>% filter(id == 1), mapping = aes(x = year, y = emissions, label = vehicles)) +
  geom_line() + geom_label() + labs(x = "Year (Increasing Vehicles)", y = "Emission (tons)", title = "") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, color = "#373737")) +
  ggplot2::scale_x_continuous(expand = expansion(c(0.25, 0.25)))
g2 = ggplot(data = output %>% filter(id == 2), mapping = aes(x = year, y = emissions, label = vehicles)) +
  geom_line() + geom_label() + labs(x = "Year (Decreasing Vehicles)", y = NULL, title = "Year vs. Carbon Emission") +
  theme_classic() +
  theme(panel.border = element_rect(fill = NA, color = "#373737")) +
  ggplot2::scale_x_continuous(expand = expansion(c(0.25, 0.25)))
g3 = ggplot(data = output %>% filter(id == 3), mapping = aes(x = year, y = emissions, label = vmt)) +
  geom_line() + geom_label() + labs(x = "Year (Decreasing VMT)", y = NULL, title = "" ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.border = element_rect(fill = NA, color = "#373737")) +
  ggplot2::scale_x_continuous(expand = expansion(c(0.25, 0.25)))


#install.packages("ggpubr")
library(ggpubr)
gg = ggarrange(plotlist = list(g1,g2,g3), ncol = 3, nrow = 1)

gg
ggsave(plot = gg, filename = "radviz.png", dpi = 500, width = 8, height = 4)

###################################################################################################

g1 = ggplot(data = output %>% filter(id == 1), mapping = aes(x = year, y = emissions, label = vehicles)) +
  geom_line() + geom_text(hjust = 0.8, nudge_y = 0, size = 2.5) + labs(x = "Year (Increasing Vehicles)", y = "Emission (tons)", title = "")
g2 = ggplot(data = output %>% filter(id == 2), mapping = aes(x = year, y = emissions, label = vehicles)) +
  geom_line() + geom_label(label.padding = unit(0.02, "lines")) + labs(x = "Year (Decreasing Vehicles)", y = NULL, title = "Year vs. Carbon Emission")
g3 = ggplot(data = output %>% filter(id == 3), mapping = aes(x = year, y = emissions, label = vmt)) +
  geom_line() + geom_label() + labs(x = "Year (Decreasing VMT)", y = NULL, title = "" ) +
  theme(plot.title = element_text(hjust = 0.5))


#install.packages("ggpubr")
library(ggpubr)
gg = ggarrange(plotlist = list(g1,g2,g3), ncol = 3, nrow = 1)

gg
ggsave(plot = gg, filename = "radviz.png", dpi = 500, width = 8, height = 4)


f = log(emissions) ~ poly(log(vmt),3) + (vehicles) + (sourcehours) + poly(year,2) + starts
#f = log(emissions) ~ poly(log(vmt),2) + sqrt(vehicles) + sqrt(sourcehours) + year
#formula1 = log(emissions) ~ poly(log(vmt), 2) + log(vehicles) + (sourcehours) + poly(year,2))

# Compute the model
m = default %>% lm(formula = f)

data4 <- data.frame(vmt = 1527484+50000,
                    vehicles = 45,
                    sourcehours = 100866.5,
                    year = 2020,
                    starts = 45*1300)

# Calculate predicted emissions using the model
data4$predicted_emissions <- exp(predict(m, newdata = data4))


output = project(
  .newx = list(year = 2020, vmt = 1527484+50000, vehicles = 45, sourcehours = 100866.5, starts = 45*1300),
  m = m, data = default, .cats = "year", .exclude = "geoid", .context = FALSE)

output
