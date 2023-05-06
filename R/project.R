#' @name project()
#' 
#' @description Function to generate data.frames of projected emissions given
#' @export

project = function(m, data, .newx, .cats = "year", .outcome = "emissions", .exclude = "geoid"){
  # Example
  # .newx = tibble(year = 2020, vmt = 200)
  
  source("R/setx.R")
  
  # Generate newdata for comparison
  newdata = setx(data, .newx, .cats = .cats, .exclude = .exclude, .outcome = .outcome)
  
  custom = newdata %>% filter(type == "custom")
  
  default = newdata %>% filter(type == "default")
  
  # Filter just to the custom data
  custom = custom %>%
    # Get predictions
    mutate(predict(m, ., se.fit = TRUE, ci = 0.95) %>% 
             as_tibble() %>% 
             # Including emissions, standard error, df, and sigma
             select(emissions = fit, se = se.fit, df, sigma = residual.scale)) 
  
  # Bind back together
  output = bind_rows(custom, default)
    
  # Return output
  return(output)
  
}
