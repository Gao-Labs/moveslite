#' @name diagnose()
#' @title Diagnostics for a model based on formula
#' @description For any formula, evaluate its goodness of fit and return identifiers.
#' @importFrom dplyr %>% mutate select
#' @importFrom purrr possibly
#' @importFrom stats lm
#' @importFrom broom glance
#' @export

diagnose <- function(.data, .formula, .pollutant, .by, .geoid) {
  library(dplyr)
  library(broom)

  # Write the central function...
  diagnose_it = function(.data, .formula, .pollutant, .by, .geoid){
    # Make model
    lm(formula = .formula, data = .data) %>%
      # Extract GOF stats
      broom::glance() %>%
      # Label by formula
      mutate(formula = paste0(as.character(.formula)[2], "~", as.character(.formula)[3])) %>%
      # Label by geoid, pollutant, and by
      mutate(geoid = .geoid, pollutant = .pollutant, by = .by) %>%
      # Grab these diagnostics and columns
      select(geoid,by,pollutant, adjr = adj.r.squared, sigma, df.residual, formula)
  }

  # Create an error-proof function...
  try_diagnostic = purrr::possibly(
    # Try making a model with that formula
    .f = ~diagnose_it(.data = .data, .formula = .formula, .pollutant = .pollutant, .by = .by, .geoid = .geoid),
    # Output an empty tibble if it doesn't work.
    otherwise = tibble(adjr = NA, sigma = NA, df.residual = NA, formula = as.character(.formula)[3]))

  # Implement the error-proof function
  result = try_diagnostic(.data = .data, .formula = .formula, .pollutant = .pollutant, .by = .by, .geoid = .geoid)

  return(result)
}
