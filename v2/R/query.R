#' @name check_status
#' @title check_status()
#' @author Tim Fraser
#' @description
#' Function to query the CAT Public API and check the status of CATSERVER.
#' Handy helper function that, if you run before using the optimizer,
#' will ensure that the API is warmed up and ready to go for you.
#' @importFrom httr GET add_headers config timeout
#' @importFrom readr read_csv
#' @export
check_status = function(){
  base = "https://api.cat-apps.com/"
  endpoint = "status/"
  # Build full URL
  url = paste0(base, endpoint)
  # Add Header
  headers = add_headers("Content-Type" = "text/csv")
  # Send it!
  # Make the request timeout after 10 seconds.
  result = GET(url = url, headers, encode = "json", config(timeout(10)))
  # Convert from raw to character
  output = rawToChar(result$content)
  # Parse as csv
  output = read_csv(output, show_col_types = FALSE)
  # Return
  return(output)
}

#' @name query
#' @title query()
#' @author Tim Fraser
#' @description Function to query catserver using CAT Public API.
#* @param geoid Unique 5-digit county or 2-digit state geoid. Example: "36109" is Tompkins County, NY.
#* @param pollutant EPA pollutant code from MOVES software. Example: 98 is CO2 Equivalent Emissions.
#* @param aggregation ID of Aggregation Level (overall = `16`, by sourcetype = `8`, by fueltype = `14`, by regulatory class = `12`, overall with sourcetype = `17`, overall with regulatory class = `18`, overall with fueltype = `19`, overall with roadtype = `20`, overall with sourcetype and fueltype = `21`)
#* @param var Name(s) of variables to return for MOVESLite analysis.
#* @param sourcetype EPA sourcetype ID.
#* @param regclass EPA regulatory class ID.
#* @param fueltype EPA fueltype ID.
#* @param roadtype EPA roadtype ID.
#' @importFrom httr GET add_headers
#' @importFrom readr read_csv
query = function(geoid = "36109",
                  pollutant = 98,
                  aggregation = 16,
                  var = c("year", "vmt", "vehicles", "starts", "sourcehours"),
                  sourcetype = NA,
                  regclass = NA,
                  fueltype = NA,
                  roadtype = NA){
  # Testing values
  # geoid = "36109"; pollutant = 98; aggregation = 16; var = c("year", "vmt", "vehicles", "starts", "sourcehours"); sourcetype = NA; regclass = NA; fueltype = NA; roadtype = NA
  # geoid = "36109"; pollutant = 98; aggregation = 8; var = c("year", "vmt", "vehicles", "starts", "sourcehours"); sourcetype = 21; regclass = NA; fueltype = NA; roadtype = NA
  # geoid = "36109"; pollutant = 98; aggregation = 8; var = c("year", "vmt", "vehicles", "starts", "sourcehours"); sourcetype = c(21,31); regclass = NA; fueltype = NA; roadtype = NA

  # Get base URL for api and endpoint
  base = "https://api.cat-apps.com/"
  endpoint = "moveslite/v1/retrieve_data/"

  query = paste0("?geoid=", geoid)
  if(!is.na(pollutant)){ query = paste0(query, "&pollutant=", pollutant)}
  if(!is.na(aggregation)){ query = paste0(query, "&aggregation=", aggregation) }
  # For as many IDs as are provided, concatenate them...
  if(any(!is.na(sourcetype))){ query = paste0(query, paste0(paste0("&sourcetype=", sourcetype), collapse = ""))}
  if(any(!is.na(fueltype))){ query = paste0(query, paste0(paste0("&fueltype=", fueltype), collapse = ""))}
  if(any(!is.na(regclass))){ query = paste0(query, paste0(paste0("&regclass=", regclass), collapse = ""))}
  if(any(!is.na(roadtype)) ){ query = paste0(query, paste0(paste0("&roadtype=", roadtype), collapse = ""))}
  # For as many variables as are provided...
  if(any(!is.na(var)) ){  query = paste0(query,   paste0( paste0("&var=", var), collapse = "")) }

  # Build full URL
  url = paste0(base, endpoint, query)

  # Add Header
  headers = add_headers("Content-Type" = "text/csv")

  # Send it! Make the request time out after 10 seconds.
  result = GET(url = url, headers, encode = "json", config(timeout(10)))

  # Convert from raw to character
  output = rawToChar(result$content)

  # If successful query, return the csv. Otherwise, return the query as a whole
  if(result$status_code == 200){
    # Parse as csv
    output = read_csv(output, show_col_types = FALSE)
    # Return
    return(output)

  }else{ return (result) }

}



#' @name get_default
#' @title `get_default()`
#' @author Tim
#' @description
#' Produces `default()` input data for a given CATSERVER query.
#' This data.frame is used to populate the original table and makes the benchmark scenario in the lineplot.
#' @param .scenario eg. granddata.d36109
#' @param .pollutant eg. 98
#' @param .by eg. 8.41 --> look at moveslite::choices_aggregation for values. Some are single values (by = 16), while others are combinations like 8.41 (by = 8 for sourcetype 41)
#'
#' @importFrom dplyr `%>%` select any_of
#' @importFrom stringr str_remove str_split
#'
#' @export
get_default = function(.scenario = "granddata.d36109", .pollutant = 98, .by = "8.41"){
  # Testing values
  # input = list(scenario = "granddata.d36109", pollutant = 98, by = choices_aggregation[[2]][5])
  # input = list(scenario = "granddata.dXXXXX", pollutant = 98, by = choices_aggregation[[2]][5])

  # .scenario = "granddata.d36109"; .pollutant = 98; .by = "8.41"

  # Get data
  # .scenario = input$scenario
  # .pollutant = input$pollutant
  # .by = input$by

  # Derive inputs
  .dbname = stringr::str_remove(.scenario, pattern = "[.].*")
  .table = stringr::str_remove(.scenario, pattern = paste0(.dbname, "[.]"))
  .geoid = stringr::str_extract(.table, pattern = "d[0-9]+") %>% stringr::str_remove("d")

  .byvalues = stringr::str_split(string = .by, pattern = "[.]") %>% unlist()
  .byid = as.integer(.byvalues[[1]])
  # Build initial filter
  .filters = list(.pollutant = .pollutant, .by = .byid,
                  .sourcetype = NA_integer_,
                  .fueltype = NA_integer_,
                  .regclass = NA_integer_,
                  .roadtype = NA_integer_)
  # If there are 2 byvalues, grab the second; otherwise, leave it null
  if(length(.byvalues) > 1){

    .bytype = as.integer(.byvalues[[2]])
    if(.byid == 14){ .filters$.fueltype = .bytype }
    if(.byid == 8){ .filters$.sourcetype = .bytype }
    if(.byid == 15){ .filters$.roadtype = .bytype }
    if(.byid == 12){ .filters$.regclass = .bytype }
  }

  # Query CATSERVER via API
  output = query(geoid = .geoid, pollutant = .filters$.pollutant, aggregation = .filters$.by,
         sourcetype = .filters$.sourcetype, fueltype = .filters$fueltype,
         regclass = .filters$regclass, roadtype = .filters$roadtype)

  output = output %>%
    select(-any_of("geoid"))

  return(output)
}


