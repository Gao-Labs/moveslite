#' @name setx()
#'
#' @param data data.frame of default data collected from CAT Grand Database
#' @param .newx data.frame, named vector, or list of new values for 1 or more x variables.
#' @param .cats vector of stratifying variable names, for which we should get estimates for each (eg. year) 
#' @param .exclude vector of variable names to exclude from data.frame - eg. id variables
#' @description Function to create a data.frame of `newdata` to pass to a model, filling in the rest with default `data`.
#' @importFrom dplyr %>% filter mutate across if_any if_all any_of all_of

setx = function(data, .newx, .cats = "year", .outcome = "emissions", .exclude = c("geoid"), .forecast = TRUE){

  # Examples for testing:
  # .newx = c(year = 2021, vmt = 200)
  # .cats = "year"
  # .exclude = "geoid"
  # .forecast = TRUE
  
  # If it's a vector, convert it to a list
  if(is.vector(.newx)){ .newx = as.list(.newx)}
  # If it's a list object, convert it to a data.frame
  if(is.list(.newx)){ .newx = dplyr::as_tibble(.newx)  }
  
  # Exclude any variables
  d = data %>% select(-any_of(.exclude))
  
  # Get variables included in your data query (other than .geoid)
  .vars = names(d) 
  
  # Get any categorical variables, in this case, year
  # eg. .cats = "year"
  
  # Get numeric xvars your data.frame supplied  
  .xvars = .newx %>% names() %>% .[!. %in% c(.cats, .outcome) ]
  
  # Get xvars your data.frame did NOT supply
  .otherxvars = .vars[!.vars %in% c(.xvars, .cats, .outcome) ]
  
  
  # Make a default data.frame
  default = d 
  
  # For each categorical variable shared, 
  # Check first, do I have default data already for those values?
  check = default
  for(i in .cats){ check = check %>% filter(!!sym(i) %in% .newx[[i]]) }
  # Were you actually able to get default data for those values?
  condition = nrow(check) > 0
  
  
  # If default data for those provided values exists, just use that!
  if(condition == TRUE){
    default = check
  }else{
    # Otherwise, interpolate the values of each missing predictor using the supplied data.
    # Make a data.frame of custom data, which we'll fill in
    default = .newx 
  }
  
  # If forecasting is desired...
  if(.forecast == TRUE){
    # Expand over time  
    default = default %>% 
      reframe(
        year = seq(from = year, to = 2060, by = 1),
        across(.cols = everything(), .fns= ~.x))
    # Otherwise, just keep as is.
  }else{ .newx_range = .newx }
  
  # For each variable NOT supplied, we're going to interpolate its value based on the year supplied  
  for(i in c(.outcome, .xvars, .otherxvars) ){
    # Create a linear interpolation function for the variable i, predicting it as best as possible 
    f = data %>% select(outcome = all_of(i), year) %>% lm(formula = outcome ~ poly(year, 3) )
    # Predict xvalues based on time trend
    default = default %>% mutate(!!sym(i) := predict(f, .))
  }

  
  
  # Latest Year
  latest = max(.newx$year)
  diff = default %>% filter(year == latest)
  # Compute the difference (new - default) 
  for(i in .xvars){ diff = diff %>% mutate(!!sym(i) := filter(.newx, year == latest)[[i]] - !!sym(i)  ) }
  
  if(.forecast == TRUE){
  diff = diff %>% 
    reframe(year = seq(from = year, to = 2060, by = 1),
            across(.cols = everything(), .fns= ~.x) )
  }
  
  custom = default
  for(i in c(.xvars)){
    custom = custom %>% 
      left_join(by = .cats, y = diff %>% select(any_of(.cats), change = i)) %>% 
      mutate(!!sym(i) := !!sym(i) + change) %>%
      select(-change)
  }

  
  # How much different is the vmt in custom than in default?

  # Bind these together and label.
  output = bind_rows(default, custom, .id = "type") %>% 
    mutate(type = type %>% dplyr::recode_factor("1" = "benchmark", "2" = "custom"))
  
  return(output)
}
