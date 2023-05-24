#' @title diagnostic workflow # 8
#' @author Yan
#' @describeIn diagnostic workflow with model 8


# model = lm(emissions ~ vmt + poly(vehicles, 2) +
#         poly(sourcehours, 2) + poly(starts, 2) + year, data = default)



rm(list = ls())



library(dplyr)
library(broom)
library(readr)
library(purrr)

#' These are the 5 core functions in `moveslite`.
source("R/connect.R")
source("R/query.R")
source("R/setx.R")
source("R/estimate.R")
source("R/project.R")

db = connect("data")
# Get just the full vector of table names that are counties (5 digits)
alltables = tibble(tables = db %>% dbListTables()) %>%
  filter(stringr::str_detect(tables, pattern = "d[0-9]{5}")) %>%
  with(tables)

#####################################################################
################   By = 16  #########################################
#####################################################################

diagnostic <- function(.data, .formula, .pollutant, .by, .geoid) {
  library(dplyr)
  library(broom)
  result <- NULL

  tryCatch(
    result <- lm(formula = .formula, data = .data) %>%
      glance() %>%
      mutate(formula = as.character(.formula)[3]) %>%
      mutate(geoid = .geoid,
             pollutant = .pollutant,
             by = .by) %>%
      select(geoid,by,pollutant,adjr = adj.r.squared, sigma, df.residual, formula),
    error = function(e) {
      output <- tibble(adjr = NA, sigma = NA, df.residual = NA, formula = as.character(.formula)[3])
      return(output)
    }
  )

  return(result)
}


table_list <- list()

for (i in 1:3110) {

  .table = alltables[i]

  .pollutant = c("CO2e" = 98)
  .by = c("Overall" = 16)
  # Make filters and list variables
  .filters = c(.pollutant = unname(.pollutant), .by = unname(.by) )
  .vars = c("year", "vmt", "vehicles", "sourcehours", "starts")

  # Download data (should end up with ~14 rows)
  default = query(.db = db, .table = .table, .filters = .filters, .vars = .vars)

  table_list[[i]] <- diagnostic(.data = default, .formula = emissions ~ vmt + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year, .pollutant = .pollutant, .by = .by, .geoid = .table)

}

by_16_8 <- do.call(rbind, table_list)  # Using rbind()

by_16_8 %>%
  write_csv("yan/model8/table_by_16_8.csv")


#####################################################################
################   By = 8  ##########################################
####### type = 11, 21, 32, 32, 41, 42, 43, 51, 52, 53, 54, 61, 62 ###
#####################################################################

sourcetype_1 <- c(11, 21, 31, 32, 41, 42, 43, 51, 52, 53, 54, 61, 62)

diagnostic <- function(.data, .formula, .pollutant, .by, .geoid, .type) {
  library(dplyr)
  library(broom)
  result <- NULL

  tryCatch(
    result <- lm(formula = .formula, data = .data) %>%
      glance() %>%
      mutate(formula = as.character(.formula)[3]) %>%
      mutate(geoid = .geoid,
             pollutant = .pollutant,
             by = .by,
             type = .type) %>%
      select(geoid,by,type,pollutant,adjr = adj.r.squared, sigma, df.residual, formula),
    error = function(e) {
      output <- tibble(adjr = NA, sigma = NA, df.residual = NA, formula = as.character(.formula)[3])
      return(output)
    }
  )

  return(result)
}


table_list <- list()

for (i in 1:3110) {

  .table = alltables[i]

  .pollutant = c("CO2e" = 98)
  .by = c("Overall" = 8)
  # Make filters and list variables
  .filters = c(.pollutant = unname(.pollutant), .by = unname(.by) )
  .vars = c("year", "vmt", "vehicles", "sourcehours", "starts", "sourcetype")

  for (j in 1:13) {

    default = query(.db = db, .table = .table, .filters = .filters, .vars = .vars) %>%
      filter (sourcetype == sourcetype_1[j])
    result <- diagnostic(.data = default, .formula = emissions ~ vmt + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year, .pollutant = .pollutant, .by = .by, .geoid = .table, .type = sourcetype_1[j])
    table_list <- append(table_list, list(result))
  }

}

by_8_8 <- do.call(rbind, table_list)  # Using rbind()

by_8_8 %>%
  write_csv("yan/model8/table_by_8_8.csv")

#####################################################################
################   By = 14  #########################################
####### type = 1, 2, 3, 4, 5, 9  ####################################
#####################################################################

fueltype_1 <- c(1, 2, 3, 4, 5, 9)

table_list <- list()

for (i in 1:3110) {

  .table = alltables[i]

  .pollutant = c("CO2e" = 98)
  .by = c("Overall" = 14)
  # Make filters and list variables
  .filters = c(.pollutant = unname(.pollutant), .by = unname(.by) )
  .vars = c("year", "vmt", "vehicles", "sourcehours", "starts", "fueltype")

  for (j in 1:6) {

    default = query(.db = db, .table = .table, .filters = .filters, .vars = .vars) %>%
      filter (fueltype == fueltype_1[j])
    result <- diagnostic(.data = default, .formula = emissions ~ vmt + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year, .pollutant = .pollutant, .by = .by, .geoid = .table, .type = fueltype_1[j])
    table_list <- append(table_list, list(result))
    }

}

by_14_8 <- do.call(rbind, table_list)  # Using rbind()
by_14_8 %>%
  write_csv("yan/model8/table_by_14_8.csv")


#####################################################################
################   By = 15  #########################################
####### type = 1, 2, 3, 4, 5  #######################################
#####################################################################

roadtype_1 <- c(1, 2, 3, 4, 5)

table_list <- list()

for (i in 1:3110) {

  .table = alltables[i]

  .pollutant = c("CO2e" = 98)
  .by = c("Overall" = 15)
  # Make filters and list variables
  .filters = c(.pollutant = unname(.pollutant), .by = unname(.by) )
  .vars = c("year", "vmt", "vehicles", "sourcehours", "starts", "roadtype")

  for (j in 1:5) {

    default = query(.db = db, .table = .table, .filters = .filters, .vars = .vars) %>%
      filter (roadtype == roadtype_1[j])
    result <- diagnostic(.data = default, .formula = emissions ~ vmt + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year, .pollutant = .pollutant, .by = .by, .geoid = .table, .type = roadtype_1[j])
    table_list <- append(table_list, list(result))
  }

}

by_15_8 <- do.call(rbind, table_list)  # Using rbind()

by_15_8 %>%
  write_csv("yan/model8/table_by_15_8.csv")

#####################################################################
################   By = 12  #########################################
####### type = 10, 20, 30, 41, 42, 46, 47, 48, 49  ##################
#####################################################################

regclass_1 <- c(10, 20, 30, 41, 42, 46, 47, 48, 49)

table_list <- list()

for (i in 1:3110) {

  .table = alltables[i]

  .pollutant = c("CO2e" = 98)
  .by = c("Overall" = 12)
  # Make filters and list variables
  .filters = c(.pollutant = unname(.pollutant), .by = unname(.by) )
  .vars = c("year", "vmt", "vehicles", "sourcehours", "starts", "regclass")

  for (j in 1:9) {

    default = query(.db = db, .table = .table, .filters = .filters, .vars = .vars) %>%
      filter (regclass == regclass_1[j])
    result <- diagnostic(.data = default, .formula = emissions ~ vmt + poly(vehicles, 2) + poly(sourcehours, 2) + poly(starts, 2) + year, .pollutant = .pollutant, .by = .by, .geoid = .table, .type = regclass_1[j])
    table_list <- append(table_list, list(result))
  }

}

by_12_8 <- do.call(rbind, table_list)  # Using rbind()

by_12_8 %>%
  write_csv("yan/model8/table_by_12_8.csv")

##########################################################################################

dbDisconnect(db); remove(db)
