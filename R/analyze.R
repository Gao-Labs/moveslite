#' @name analyze()
#' 
#' @description Major wrapper for most of workflow process.

analyze = function(
    .geoid = "36109",
    .filters = c(.by = 16, .pollutant = 98), 
    .vars = c("vmt", "vehicles", "starts", "sourcehours", "year"),
    .newx = tibble(year = 2020, vmt = 1000),
    .outcome = "emissions",
    .cats = "year",
    .exclude = "geoid"
    ){
  
  # Example Inputs
  # .geoid = "36109"
  # .filters = c(.by = 16, .pollutant = 98)
  # .vars = c("vmt", "vehicles", "starts", "sourcehours", "year")
  # .newx = tibble(year = 2020, vmt = 10000)
  # .forecast = TRUE
  # .outcome = 'emissions'
  # .cats = "year"
  # .exclude = "geoid"
  # 
  # Load functions  
  source("R/connect.R")
  source("R/table_exists.R")
  source("R/query.R")
  source("R/setx.R")
  source("R/estimate.R")
  source("R/project.R")
  
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
  
  
  stat = project(m, data, .newx = .newx,
          .cats = .cats, .outcome = .outcome, .exclude = .exclude, .forecast = .forecast)
  
  stat %>% 
    ggplot(mapping = aes(color = type, x = year, y = emissions)) +
    geom_point()
  
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
