#' @name find_transformation()
#' @author Tim Fraser
#' @description Function to find the transformation used on the model outcome, if any.
#'
#' @param m model object
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect

find_transformation = function(m){

  # Load transformation data
  load("data/transformations.rda")

  # Get first part of model formula (before ~)
  outcome = names(m$model)[1]

  # Check if the outcome formula has any transformations
  outcometype =  dplyr::case_when(
    stringr::str_detect(outcome, pattern = transformations$log$pattern) ~ transformations$log,
    stringr::str_detect(outcome, pattern = transformations$log10$pattern) ~ transformations$log10,
    stringr::str_detect(outcome, pattern = transformations$sqrt$pattern) ~ transformations$sqrt,
    stringr::str_detect(outcome, pattern = transformations$cubert$pattern) ~ transformations$cubert,
    stringr::str_detect(outcome, pattern = transformations$squared$pattern) ~ transformations$squared,
    stringr::str_detect(outcome, pattern = transformations$cubed$pattern) ~ transformations$cubed,
    TRUE ~ transformations$asis
  )

  return(outcometype)

}
