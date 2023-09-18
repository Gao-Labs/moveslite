#' @name project()
#'
#' @description Function to generate data.frames of projected emissions given
#' @importFrom dplyr `%>%` select mutate as_tibble filter
#' @export

project = function(m, data, .newx, .cats = "year", .exclude = "geoid", .context = TRUE, .ci = 0.95){
  # Example

  #.newx = tibble(year = 2020, vmt = 200)

  # .exclude = "geoid"
  # .outcome = "emissions"
  # .context = TRUE
  # .cats = "year"

  source("R/setx.R")
  source("R/convert.R")
  source("R/find_transformation.R")
  load("data/transformations.rda")

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
      rowwise() %>%
      # Back-transform the estimates and the standard error
      mutate(convert(y = emissions, se = se, df = df, backtrans = otype$backtrans, ci = .ci)) %>%
      ungroup()
  }

  # Grab all default-derived data
  benchmark = newdata %>% filter(type != "custom")

  # Bind back together
  output = bind_rows(custom, benchmark)

  # Return output
  return(output)

}
