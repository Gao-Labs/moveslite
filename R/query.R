#' @name query
#' @title query
#' @description Query the CAT grand database for a specific slice of data.
#' @author Tim Fraser & Yan Guo
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
#' @importFrom dplyr `%>%` tbl filter collect any_of
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom base name as.list unique
#' @export

query = function(
    .db, .table,
    .filters = c(.by = 16, .pollutant = 98),
    .vars = c("year", "vmt", "vehicles", "starts", "sourcehours")){

  # Testing data
  #.db = connect("anydata")
  # .table = "granddata.d36109"
  # .filters = c(.by = 16, .pollutant = 98)
  # .vars = c("year", "vmt", "vehicles", "starts", "sourcehours")

  # Convert vector into a list object.
  f = .filters %>% as.list()
  # Get filtering variables named in your list
  v = names(f)


  # Find Specific table
  q = .db %>% dplyr::tbl(.table)

  # Filter by pollutant
  if(".pollutant" %in% v){  q = q %>% dplyr::filter(pollutant  %in% !!f$.pollutant)  }

  # Filter by aggregation level
  if(".by" %in%  v){ q = q %>% dplyr::filter(by %in% !!f$.by) }

  # Filter by sourcetype
  if(".sourcetype" %in% v){ q = q %>% dplyr::filter(sourcetype %in% !!f$.sourcetype) }

  # Filter by regclass
  if(".regclass" %in% v){ q = q %>% dplyr::filter(regclass  %in% !!f$.regclass) }

  # Filter by roadtype
  if(".roadtype" %in% v){ q = q %>% dplyr::filter(roadtype  %in% !!f$.roadtype) }

  # Filter by fueltype
  if(".fueltype" %in% v){ q = q %>% dplyr::filter(fueltype  %in% !!f$.fueltype) }


  # Subset to just the variables needed
  q = q %>% dplyr::select(dplyr::any_of(unique(c("geoid", "year", "emissions", .vars))))

  # Collect the data
  data = q %>% dplyr::collect()

  # Return the output
  return(data)
}
