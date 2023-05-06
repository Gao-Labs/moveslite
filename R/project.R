#' @name project()
#' 
#' @description Function to generate data.frames of projected emissions given
#' @export

project = function(m, data, .newx, .cats = "year", .outcome = "emissions", .exclude = "geoid", .forecast = TRUE){
  # Example
  #.newx = tibble(year = 2020, vmt = 200)
  
  # .exclude = "geoid"
  # .outcome = "emissions"
  # .cats = "year"
  # .forecast = TRUE
  
  source("R/setx.R")
  
  # Generate newdata for comparison
  newdata = setx(data, .newx, .cats = .cats, .exclude = .exclude, .outcome = .outcome, .forecast = .forecast)
  
  custom = newdata %>% filter(type == "custom")
  
  benchmark = newdata %>% filter(type == "benchmark")
  

  # Filter just to the custom data
  custom = custom %>%
    # Get predictions
    mutate(predict(m, ., se.fit = TRUE, ci = 0.95) %>% 
             as_tibble() %>% 
             # Including emissions, standard error, df, and sigma
             select(emissions = fit, se = se.fit, df, sigma = residual.scale)) 
  
  # Get the raw data too
  raw = data %>% mutate(type = "raw") %>% select(-any_of(.exclude))
  
  
  # Bind back together
  output = bind_rows(custom, benchmark, raw)
  
  # Return output
  return(output)
  
}
