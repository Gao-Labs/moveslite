#' @name query_many
#' @title query_many
#' @description Query multiple subsets
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
#' @importFrom dplyr `%>%` tribble mutate filter left_join select any_of
#' @export

query_many = function(.db, .table = "d36109", .filters = list(.pollutant = 98, .by = 17), .vars = c("vmt", "vehicles", "sourcehours", "starts")){

  # Inputs for testing only
  # source("R/connect.R")
  # source("R/query.R")
  # source("R/query_aggregate.R")
  # db = connect("data")
  # .by = 18
  # .pollutant = 98
  # .table = "d36109"
  # .vars = c("vmt", "vehicles", "sourcehours", "starts")

  # Testing values
  # .filters = list(.pollutant = 98, .by = 8, .sourcetype = 41)
  # .db = connect("granddata")
  # .vars = c("vmt", "vehicles", "sourcehours", "starts")
  # .table = "d36109"

  # Record original by submission
  .by = .filters$.by


  if(.by <= 16){ bycombos = .by }else{
    bycombos = switch(
      EXPR = as.character(.by),
      "17" = c(16, 8),
      "18" = c(16, 12),
      "19" = c(16, 14),
      "20" = c(16, 15),
      "21" = c(16, 8, 14)
    )
  }

  traits = dplyr::tribble(
    ~id, ~name,        ~var,
    16,  "overall",   'vehicles',
    8,  "sourcetype", 'vehicles',
    14, "fueltype",   'vehicles',
    15, "roadtype",   'vmt',
    12, "regclass",   'vehicles')

  # Get your traits
  .traits = traits %>% dplyr::filter(id %in% bycombos)

  # .vars = c("vmt", "vehicles", "sourcehours", "starts")

  # Add or Overwrite .by with the result of .traits$id
  .filters$.by = .traits$id

  # Get names of existing filters
  # fnames = names(.filters)
  # nf = length(fnames)
  # # Overwrite AND name those cells
  # for(i in 1:nf){ f[[fnames[i] ]] <- .filters[[ fnames[i] ]]  }

  # Get these variables at EACH LEVEL OF AGGREAGTION NEEDED
  dall = query(
    .db = .db, .table = .table,
    .filters = .filters,
    .vars = c("by", .traits$name, "emisions", .vars))

  # .by = 17 # Overall with Sourcetype
  if(.by %in% c(17, 18, 19, 20, 21) ){

    # Get the overall version
    doverall = dall %>%
      dplyr::filter(by == 16) %>%
      dplyr::select(dplyr::any_of(c("geoid", "year", "emissions", .vars)))

    # Find any NON-16 aggregation levels
    byother = bycombos[bycombos != 16]

    # If any...
    if(length(byother) > 0){
      # For each element in bycombos...
      for(i in byother){
        # Get the aggregate/relabeled data
        dextra = dall %>% query_aggregate(.by = i)

        # Join them together
        doverall = doverall %>% dplyr::left_join(by = c("year", "geoid"), y = dextra)

      }
    }

    # Name the end product, 'result'
    result = doverall

  # Otherwise, if by <= 16,
  }else{ result = dall %>% dplyr::select(dplyr::any_of(c("geoid", "year", "emissions", .vars)))  }

  result = result %>% ungroup()

  # Return the result
  return(result)

}
