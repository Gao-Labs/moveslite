#' @name project
#' @title project
#' @description Function to generate data.frames of projected emissions given
#' @author Tim Fraser & Yan Guo
#' @param m (character) "m" = model object
#' @param data data.frame of default data collected from CAT Grand Database
#' @param .newx data.frame, named vector, or list of new values for 1 or more x variables.
#' @param .cats vector of stratifying variable names, for which we should get estimates for each (eg. year)
#' @param .exclude vector of variable names to exclude from data.frame - eg. id variables
#' @importFrom dplyr `%>%` select mutate as_tibble filter rowwise ungroup
#' @export

project = function(m, data, .newx, .cats = "year", .exclude = "geoid", .context = TRUE, .ci = 0.95){
  # Example

  # .newx = tibble(year = 2020, vmt = 2276674)
  # m = model()
  # data = default()
  # .newx = newx()

  # .exclude = "geoid"
  # .context = FALSE
  # .cats = "year"
  # .ci = 0.95

  # .outcome = "emissions"

  # source("R/setx.R")
  # source("R/convert.R")
  # source("R/find_transformation.R")
  # load("data/transformations.rda")

  # Generate newdata for comparison
  newdata = setx(data, .newx, .cats = .cats, .exclude = .exclude, .context = .context)

  get_predictions = function(...){
    output = predict(...)

    result = output$fit %>% dplyr::as_tibble() %>%
      dplyr::select(emissions = fit, lower = lwr, upper = upr) %>%
      dplyr::mutate(se = output$se.fit,
             df = output$df,
             sigma = output$residual.scale)
    return(result)
  }

  # Add predictions for the custom data
  custom = newdata %>%
    # Filter just to the custom data
    dplyr::filter(type == "custom") %>%
    # Get predictions
    dplyr::mutate(get_predictions(m, newdata = ., se.fit = TRUE, ci = .ci, interval = "confidence", type = "response"))


  # Check whether the outcome is transformed or not
  otype = find_transformation(m = m)


  # If outcometype is NOT asis, we're going to
  # 1. transform predicted emissions and
  # 2. simulate the corrected sigma and
  if(otype$trans != "asis"){
    # Take our custom estimates and..
    custom = custom %>%
      # For each estimate...
      dplyr::rowwise() %>%
      # Back-transform the estimates and the standard error
      dplyr::mutate(convert(y = emissions, se = se, df = df, backtrans = otype$backtrans, ci = .ci)) %>%
      dplyr::ungroup()
  }

  # Grab all default-derived data
  benchmark = newdata %>% dplyr::filter(type != "custom")

  # Bind back together
  output = dplyr::bind_rows(custom, benchmark)

  # Return output
  return(output)

}
