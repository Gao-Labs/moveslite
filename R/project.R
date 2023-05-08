#' @name project()
#' 
#' @description Function to generate data.frames of projected emissions given
#' @export

project = function(m, data, .newx, .cats = "year", .exclude = "geoid", .forecast = TRUE){
  # Example
  #.newx = tibble(year = 2020, vmt = 200)
  
  # .exclude = "geoid"
  # .outcome = "emissions"
  # .context = TRUE
  # .cats = "year"
  # .forecast = TRUE
  
  
  source("R/setx.R")
  
  # Generate newdata for comparison
  newdata = setx(data, .newx, .cats = .cats, .exclude = .exclude, .context = .context)

  # Add predictions for the custom data
  custom = custom %>%
    # Filter just to the custom data
    filter(type == "custom") %>%
    # Get predictions
    mutate(predict(m, ., se.fit = TRUE, ci = 0.95) %>% 
             as_tibble() %>% 
             # Including emissions, standard error, df, and sigma
             select(emissions = fit, se = se.fit, df, sigma = residual.scale)) 
  
  # Grab all default-derived data
  benchmark = newdata %>% filter(type != "custom")
  
  
  # Bind back together
  output = bind_rows(custom, benchmark)
  
  # Return output
  return(output)
  
}
