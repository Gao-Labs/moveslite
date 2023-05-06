#' @name estimate()
#' @title Estimate your area-specific model
#' @param .filters (vector/list) Named Vector or List of inputted values for filtering.
#' @param .vars (character vector) Vector of variables to use as predictors.
#' @param .check (logical) Check the R2 of the model and print warning if below 95%? (TRUE/FALSE)
#' @importFrom dplyr %>% filter tbl
#' @importFrom broom glance
#' @importFrom stats lm
#' @note An example filter would be: `.filters = c(.by = 8, .pollutant = 98, .sourcetype = 42)`
#' @export

estimate = function(data, .vars = c("vmt", "vehicles", "starts", "sourcehours", "year"), .check = FALSE){

  ####################################
  # EDITS NEEDED
  ###################################
  # -- Do a few versions of this model to account for different numbers of .vars
  
  # Compute the model
  m = data %>% lm(formula = emissions ~ poly(vmt, 3) + vehicles + starts + sourcehours + year)
  
  # If Diagnostic checks are desired, will evaluate
  if(.check == TRUE){
    # Use broom to view the statistics
    stat = broom::glance(m)
    # Make a warning if the model is not very accurate.
    if(stat$r.squared < 0.95){ print("Warning: Model is < 95% accurate. R2 = ", round(stat$r.squared, 3) ) }
  }
  
  
  # Return model.
  return(m)
  
}
