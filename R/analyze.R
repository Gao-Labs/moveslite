#' @name analyze()
#' 
#' @description Major wrapper for most of workflow process.

analyze = function(
    .geoid = "36109",
    .filters = c(.by = 16, .pollutant = 98), 
    .vars = c("vmt", "vehicles", "starts", "sourcehours", "year"),
    .setx = tibble(year = 2020, vmt = 1000)
    ){
  
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
  
  # Estimate a model
  m = estimate(data = data, .vars = .vars, .check = FALSE)
  
  
  # For any supplied values, fill in the unsupplied values with defaults from data
  xs = names(.setx)
  
  # Compare prediction to default data as is
  notxs = .vars[!.vars %in% xs]
  
  # Compare prediction to scenario II

  # Filter default data to just the years supplied for comparison
  default = data %>% 
    filter(year %in% .setx$year) %>%
    # For each year of default data, grab the variables not supplied
    select(any_of(c("year", notxs)))
  # Join in the new data
  newdata = .setx %>% left_join(by = "year", y = default)
  
  # Get predictions
  yhat = newdata %>% mutate(yhat = predict(m, .))
  
  
  
  
  # Disconnect from database
  dbDisconnect(db); gc()
  
  
}