library(dplyr)
library(DBI)
library(RSQLite)
library(stringr)

# Get names
source("R/connect.R")
data = connect("data")
geoids = data %>% dbListTables() %>% stringr::str_remove("d")
cov = connect("cov")
cov %>%
  tbl("areas") %>%
  filter(level %in% c("state", "nation","county")) %>%
  select(geoid, state, fullname) %>%
  collect() %>%
  filter(geoid %in% !!geoids) %>%
  split(.$state) %>%
  map(~set_names(.$geoid, .$fullname)) %>%
  saveRDS("appdata.rds")

dbDisconnect(data); remove(data)
dbDisconnect(cov); remove(cov)

read_rds("appdata.rds") %>%
  map_dfr(~tibble(geoid = ., label = names(.)), .id = "state") %>%
  saveRDS("areas.rds")

# pollutants = catr::pollutant
#
# by = tribble(
#   ~id, ~label,
#   16,   "Overall",
#   8,   "by Sourcetype",
#   14,   "by Fueltype",
#   12,   "by Regulatory Class",
#   15,   "by Roadtype"
# )
library(readr)
