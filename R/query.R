#' @name query()
#' @description Query the CAT grand database for a specific slice of data.
#' @param .db database connection object for CAT GRAND database or CAT formatted MOVES output data
#' @param .table Name of table in database object `.db`
#' @param .filters Named Vector or List of inputted values for filtering.
#' @param .vars Vector of variables desired
#' @note .pollutant ID of the pollutant affected
#' @note .by ID of Aggregation Level (overall = `16`, by sourcetype = `8`, by fueltype = `14`, by regulatory class = `12`)
#' @note .sourcetype ID of Sourcetype
#' @note .regclass ID of Regulatory Class
#' @note .roadtype ID of Roadtype
#' @note .fueltype ID of Fueltype
#'
#' @importFrom dplyr tbl %>% filter collect
#' @import DBI
#' @export

query = function(
    .db, .table,
    .filters = c(.by = 16, .pollutant = 98),
    .vars = c("year", "vmt", "vehicles", "starts", "sourcehours")){

  # Convert vector into a list object.
  f = .filters %>% as.list()
  # Get filtering variables named in your list
  v = names(f)

  # Find Specific table
  q = .db %>% tbl(.table)

  # Filter by pollutant
  if(".pollutant" %in% v){  q = q %>% filter(pollutant  %in% !!f$.pollutant)  }

  # Filter by aggregation level
  if(".by" %in%  v){ q = q %>% filter(by %in% !!f$.by) }

  # Filter by sourcetype
  if(".sourcetype" %in% v){ q = q %>% filter(sourcetype %in% !!f$.sourcetype) }

  # Filter by regclass
  if(".regclass" %in% v){ q = q %>% filter(regclass  %in% !!f$.regclass) }

  # Filter by roadtype
  if(".roadtype" %in% v){ q = q %>% filter(roadtype  %in% !!f$.roadtype) }

  # Filter by fueltype
  if(".fueltype" %in% v){ q = q %>% filter(fueltype  %in% !!f$.fueltype) }


  # Subset to just the variables needed
  q = q %>% select(any_of(unique(c("geoid", "year", "emissions", .vars))))

  # Collect the data
  data = q %>% collect()

  # Return the output
  return(data)
}
