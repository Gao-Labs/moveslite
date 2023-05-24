#' @name tabler()
#' @export
#'

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
