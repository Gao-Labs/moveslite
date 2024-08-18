# Example workflow

# setup.R

# Run this script to get you setup for the first time.

# Remove catr if it is already installed
remove.packages("catr")
# Install catr from source
install.packages("z/catr_0.1.0.tar.gz", type = "source")

# I'm going to teach you how to use catr to do custom inputs to MOVES runs - later!

# Connect to sqlite
db = dbConnect(RSQLite::SQLite(), "z/db.sqlite")

# Test table (Tompkins County)
t = "d36109"

db %>%
  tbl(t) %>%
  glimpse()

# by = 8 --> aggregated by sourcetype
db %>%
  tbl(t) %>%
  head(1)

dat = db %>%
  tbl(t) %>%
  # Just aggregated by sourcetype, just buses
  filter(by == 8 & sourcetype == 42) %>%
  # Just CO2e emissions
  filter(pollutant == 98) %>%
  collect()

library(ggplot2)
library(broom)

dat %>%
  ggplot(mapping = aes(x = vmt, y = emissions)) +
  geom_point()

m = dat %>% lm(formula = emissions ~ poly(vmt,4) )

m %>% glance()
# Beta coefficient which the rate of emissions per VMT

# Take roads --> vmt --> predict --> emissions
predict(m, tibble(vmt = 3))

# What is our goal? ######################
# If I'm a user and I want to know about Bus Emissions in Tompkins County (eg. Yan!)
# Right now, I need to run MOVES --> and it's a process!
# MOVES - It requires 40 input tables.
# catr - requires just 1 or more
# movesai - requires 1 number.

# Assumptions: ##############################
# -- A1. MOVES **can** validly approximate emissions
# -- A2. A Model of MOVES Default Outputs could still
#    reasonably approximate MOVES outputs from custom input data
# --> Test this assumption (A2)

# How would you test validity?

# E-D = Emissions from MOVES with DEFAULT Input Data
# E-C = Emissions from MOVES with CUSTOM Input Data (or catr)
# E-MD = Predicted Emissions from a model of MOVES with DEFAULT Input Data
# E-MC = Predicted Emissions from a model of MOVES with CUSTOM Input Data

# Z. If E-MD correlates with E-C --> AMAZING

# Goals: ##############################################
# A. Make some functions that can download CAT formatted data from DB
# B. Make some functions that can MODEL that data
# C. Make some kind of checker - is this model good enough?
#    C1. Make a comparer - compare observed to predicted emissions.
# D. Make an example workflow that can actually run Yan's project needs.


dbDisconnect(db); rm(list = ls())
