#' @name 01_sets.R
#' @author Tim
#' @description
#' Generate a data.frame containing every combination of traits we want to validate a model for.

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# Generate list of values
bind_rows(

  tribble(
    ~id, ~term, ~label,
    "11", "Motorcycle",  "Car/\nBike",
    "21", "Passenger Car", "Car/\nBike",
    "31", "Passenger Truck", "Light\nTruck",
    "32", "Light Commercial Truck", "Light\nTruck",
    "41", "Other Buses",   "Bus",
    "42", "Transit Bus",   "Bus",
    "43", "School Bus",    "Bus",
    "51", "Refuse Truck",  "Heavy\nTruck",
    "52", "Single Unit Short-haul Truck",  "Heavy\nTruck",
    "53", "Single Unit Long-haul Truck",  "Heavy\nTruck",
    "54", "Motor Home",  "Heavy\nTruck",
    "61", "Combination Short-haul Truck", "Combo\nTruck",
    "62", "Combination Long-haul Truck", "Combo\nTruck") %>%
    mutate(type = "sourcetype"),

  tribble(
    ~id, ~term,
    "10", "MC",
    "20", "LDV",
    "30", "LDT",
    "40", "LHD2b3",
    "41", "LHD34",
    "42", "LHD45",
    "46", "MHD67",
    "47", "HHD8",
    "48", "Urban Bus",
    "49", "Glider") %>%
    mutate(label = term, type = "regclass"),

  tribble(
    ~id, ~term, ~label,
    "5", "Urban Unrestricted Access", "Urban\nUnrestricted",
    "4", "Urban Restricted Access", "Urban\nRestricted",
    "3", "Rural Unrestricted Access", "Rural\nUnrestricted",
    "2", "Rural Restricted Access", "Rural\nRestricted",
    "1", "Off-Network", "Off-Network") %>%
    mutate(type = "roadtype"),


  tribble(
    ~id, ~term, ~label,
    "1", "Gasoline", "Gas",
    "2", "Diesel Fuel", "Diesel",
    "3", "Compressed Natural Gas (CNG)", "CNG",
    "4", "Liquefied Petroleum Gas (LNG)", "LNG",
    "5", "Ethanol (E-85)", "Ethanol",
    "9", "Electricity", "Electricity") %>%
    mutate(type = "fueltype"),

  tribble(
    ~id, ~term, ~label,
    "98", "CO2 Equivalent", "CO2e",
    "91", "Total Energy Consumption", "Energy Consumption",
    "1", "Total Gaseous Hydrocarbons (TGH)", "TGH",
    "5", "Methane (CH4)", "CH4",
    "90", "Atmospheric CO2", "Atmospheric CO2",
    "31", "Sulfur Dioxides (SO2)", "SO2",
    "3", "Oxides of Nitrogen (NOx)", "NOx",
    "6", "Nitrous Oxide (N20)", "N20",
    "2", "Carbon Monoxide (CO)", "CO",
    "87", "Volatile Organic Compounds", "VOC",
    "79", "Non-Methane Hydrocarbons", "NMH",
    "110", "Primary Exhaust PM2.5 - Total", "PM2.5",
    "117", "Primary PM2.5 - Tirewear Particulate", "PM2.5 - Tirewear",
    "116", "Primary PM2.5 - Brakeware Particulate", "PM2.5 - Brakewear",
    "112", "Elemental Carbon", "Elemental Carbon",
    "115", "Sulfate Particulate", "Suflate Particulate",
    "118", "Composite - NonECPM", "Composite - NonECPM",
    "119", "H20 (aerosol)", "H20",
    "100", "Primary Exhaust PM10 - Total", "PM10",
    "106", "Primary PM10 - Breakware Particulate", "PM10 - Breakware",
    "107", "Primary PM10 - Tirewear Particulate", "PM10 - Tirewear") %>%
    mutate(type = "pollutant")
) %>%
  write_csv("diagnostics/keywords.csv")

keywords <- read_csv("diagnostics/keywords.csv")

# Generate a grid of all sets - meaning all pollutant - by - type combinations.
bind_rows(
  keywords %>% filter(type == "sourcetype") %>% select(type = id) %>% mutate(by = 8),
  keywords %>% filter(type == "fueltype") %>% select(type = id) %>% mutate(by = 14),
  keywords %>% filter(type == "roadtype") %>% select(type = id) %>% mutate(by = 12),
  keywords %>% filter(type == "regclass") %>% select(type = id) %>% mutate(by = 15),
  tibble(type = 0, by = 16)
) %>%
  tidyr::expand_grid(
    pollutant = keywords %>% filter(type == "pollutant") %>% with(id)
  ) %>%
  # Add a unique id for each set
  mutate(set_id = 1:n()) %>%
  # Save to file
  write_csv("diagnostics/sets.csv")


# Get all geoids
source("R/connect.R")
db = connect("data")
# Get just the full vector of table names that are counties (5 digits)
alltables = tibble(tables = db %>% dbListTables()) %>%
  filter(stringr::str_detect(tables, pattern = "d[0-9]{5}|d[0-9]{2}")) %>%
  with(tables)
allgeoids = alltables %>% stringr::str_remove("d")
dbDisconnect(db)

# Get a grid of all geoid-set pairs
read_csv("diagnostics/sets.csv") %>%
  tidyr::expand_grid(geoid = allgeoids) %>%
  # Add a unique ID for each geoid-set run
  mutate(run_id = 1:n()) %>%
  saveRDS("diagnostics/runs.rds")

# Clear environment and cache
rm(list = ls()); gc()
