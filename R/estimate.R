#' @name estimate()
#' @title Estimate your area-specific model
#' @param .geoid (character) geoid of the area examined
#' @param .filters (vector/list) Named Vector or List of inputted values for filtering.
#' @param .vars (character vector) Vector of variables to use as predictors.
#' @param .check (logical) Check the R2 of the model and print warning if below 95%? (TRUE/FALSE)
#' @importFrom DBI dbDisconnect dbConnect
#' @importFrom dplyr %>% filter tbl
#' @importFrom broom glance
#' @importFrom stats lm
#' @note An example filter would be: `.filters = c(.by = 8, .pollutant = 98, .sourcetype = 42)`
#' @export

estimate = function(.geoid = "36109", .filters = c(.by = 16, .pollutant = 98), 
                    .vars = c("vmt", "vehicles", "starts", "sourcehours", "year"), 
                    .check = FALSE){

  # Load functions  
  source("R/connect.R")
  source("R/table_exists.R")
  source("R/query.R")
  
  # Connect to data database
  db = connect("data")
  # Construct table
  .table = paste0("d", .geoid)
  # Does table exist in this database?
  exist = table_exists(db, .table)
  
  # If table does not exists, stop.
  if(exist == FALSE){ stop("Error: Table does not exist in database.") }
  
  # Otherwise, continue
  # Query the table with the supplied filters
  data = db %>% query(.table = .table, .filters = .filters)
  
  # Compute the model
  m = data %>% lm(formula = emissions ~ poly(vmt, 3) + vehicles + starts + sourcehours + year)
  
  # If Diagnostic checks are desired, will evaluate
  if(.check == TRUE){
    # Use broom to view the statistics
    stat = broom::glance(m)
    # Make a warning if the model is not very accurate.
    if(stat$r.squared < 0.95){ print("Warning: Model is < 95% accurate. R2 = ", round(stat$r.squared, 3) ) }
  }
  
  # Disconnect from database
  dbDisconnect(db); gc()
  # Return model.
  return(m)
  
}
