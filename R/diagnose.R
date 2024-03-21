#' @name diagnose
#' @title Diagnostics for a model based on formula
#' @author Tim Fraser & Yan Guo
#' @description For any formula, evaluate its goodness of fit and return identifiers.
#' @param .data (char) ".data" = Cat-formatted MOVES grand data database
#' @param .formula (char) ".formula" = Statistical formula
#' @param .pollutant (char) ".pollutant" = type of pollutant want to make prediction on
#' @param .by (char) ".by" = by = 16 = Overall, by = 8 = Sourcetype, by = 12 = Regulatory Class, by = 14 = Fueltype, by = 15 = Roadtype
#' @param .geoid (char) ".geoid" = location/county name
#' @importFrom dplyr `%>%` mutate select tibble
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
    stats::lm(formula = .formula, data = .data) %>%
      # Extract GOF stats
      broom::glance() %>%
      # Label by formula
      dplyr::mutate(formula = paste0(as.character(.formula)[2], "~", as.character(.formula)[3])) %>%
      # Label by geoid, pollutant, and by
      dplyr::mutate(geoid = .geoid, pollutant = .pollutant, by = .by) %>%
      # Grab these diagnostics and columns
      dplyr::select(geoid,by,pollutant, adjr = adj.r.squared, sigma, df.residual, formula)
  }

  # Create an error-proof function...
  try_diagnostic = purrr::possibly(
    # Try making a model with that formula
    .f = ~diagnose_it(.data = .data, .formula = .formula, .pollutant = .pollutant, .by = .by, .geoid = .geoid),
    # Output an empty tibble if it doesn't work.
    otherwise = dplyr::tibble(adjr = NA, sigma = NA, df.residual = NA, formula = as.character(.formula)[3]))

  # Implement the error-proof function
  result = try_diagnostic(.data = .data, .formula = .formula, .pollutant = .pollutant, .by = .by, .geoid = .geoid)

  return(result)
}
