#' @name analyze()
#' 
#' @description Major wrapper for most of workflow process.

analyze = function(
    .geoid = "36109",
    .filters = c(.by = 16, .pollutant = 98), 
    .vars = c("vmt", "vehicles", "starts", "sourcehours", "year"),
    .setx = tibble(year = 2020, vmt = 1000)
    ){
  
  # Example Inputs
  # .geoid = "36109"
  # .filters = c(.by = 16, .pollutant = 98)
  # .vars = c("vmt", "vehicles", "starts", "sourcehours", "year")
  # .setx = tibble(year = 2020, vmt = 1000)
  
  # Load functions  
  source("R/connect.R")
  source("R/table_exists.R")
  source("R/query.R")
  source("R/setx.R")
  source("R/estimate.R")
  
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
  
  # Estimate a model
  m = estimate(data = data, .vars = .vars, .check = FALSE)
  
  
  stat = project(m, data, .newx = c(year = 2020, vmt = 2000),
          .cats = "year", .outcome = "emissions", .exclude = "geoid")
  
  # Calculate quantities of interest
  qis = stat %>% 
    summarize(
      across(.cols = any_of(c("emissions", .vars)), 
             .fns = ~.x[type == "default"] - .x[type == "custom"]))
  
  # Disconnect from database
  dbDisconnect(db); gc()
  
  list(stat, qis) %>%
    return()
}
