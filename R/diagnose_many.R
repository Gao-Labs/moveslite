#' @name diagnose_many
#' @title Diagnostics for **many** models based on formula
#' @description For a list of formula, evaluate each model's goodness of fit and return identifiers.
#' @importFrom dplyr %>% mutate select filter arrange bind_rows collect tbl
#' @importFrom DBI dbDisconnect dbConnect
#' @importFrom purrr possibly map
#' @importFrom stats lm
#' @importFrom broom glance
#' @export

diagnose_many = function(.geoid = "36109", .pollutant = 98, .by = 8, .type = 42, .formulas = list()){

  # Load required packages
  require(dplyr)
  require(readr)
  require(purrr)
  require(broom)

  # Load required functions
  source("R/connect.R")
  source("R/diagnose.R")

  # Connect to database
  db = connect("data")

  # Query database
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
    # Filter to cases where that variable contains that type
    data = data %>% filter(!!sym(.byvar) == .type)
  }

  # Generate a table from your list of formulas
  tab = .formulas %>%
    # Apply this function many times
    map(.f = ~diagnose(.data = data, .pollutant = .pollutant, .by = .by, .geoid = .geoid,  .formula = .x), .id = "formula_id") %>%
    # Keep only results where at least 1 adjr value is not NA.
    keep(~sum(!is.na(.x$adjr)) > 0)

  # If there are ANY remaining results,
  result = if(length(tab) > 0){
    tab %>%
      # Bind a list of data.frames together
      bind_rows(.id = "formula_id") %>%
      # Arrange in order of predictive power
      arrange(desc(adjr)) %>%
      # Add labels!
      mutate(geoid = .geoid,  pollutant = .pollutant, by = .by,
             # If you have a type, fill it in; otherwise, make it blank
             type = if_else(.by != 16, true = .type, false = NA_integer_))
  }else{
    # If there are NO remaining results, return a blank tibble
    tibble()
  }

  return(result)
}
