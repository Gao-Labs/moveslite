#' @name setx
#' @title setx
#' @param data data.frame of default data collected from CAT Grand Database
#' @author Tim Fraser & Yan Guo
#' @param .newx data.frame, named vector, or list of new values for 1 or more x variables.
#' @param .cats vector of stratifying variable names, for which we should get estimates for each (eg. year)
#' @param .exclude vector of variable names to exclude from data.frame - eg. id variables
#' @description Function to create a data.frame of `newdata` to pass to a model, filling in the rest with default `data`.
#' @importFrom dplyr `%>%` filter mutate across if_any if_all any_of all_of
#' @importFrom base is.vector as.list is.list names min
#' @importFrom stats approxfun

setx = function(data, .newx, .cats = "year", .exclude = c("geoid"), .context = TRUE){

  # Examples for testing:
  # .newx = c(year = 2021, vmt = 800000000 )
  # .cats = "year"
  # .exclude = "geoid"
  # .context = TRUE

  .outcome = "emissions"

  # If it's a vector, convert it to a list
  if(is.vector(.newx)){ .newx = as.list(.newx)}
  # If it's a list object, convert it to a data.frame
  if(is.list(.newx)){ .newx = dplyr::as_tibble(.newx)  }

  # Exclude any unneeded variables
  d = data %>% dplyr::select(-dplyr::any_of(.exclude))

  # Get variables included in your data query (other than .geoid)
  .vars = names(d)

  # Get any categorical variables, in this case, year
  # eg. .cats = "year"

  # Get numeric xvars your data.frame supplied
  .xvars = .newx %>% names() %>% .[!. %in% c(.cats, .outcome) ]

  # Get xvars your data.frame did NOT supply
  .otherxvars = .vars[!.vars %in% c(.xvars, .cats, .outcome) ]


  # Make a default data.frame

  # Use linear interpolation to jump between years
  funs = list()

  # Get all xvariables (except year)
  .allxvars = c(.outcome, .xvars, .otherxvars)

  # For each xvariable
  for(i in  1:length(.allxvars) ){
  # Generate an approximation function, which fills in the gaps between each x-y pair with linear interpolation.
  funs[[.allxvars[i]  ]] <- d %>%
    dplyr::select(year, x = .allxvars[i]) %>%
    stats::approxfun(method = "linear", na.rm = TRUE)
  }

  # Estimate the x-variable values for that year using linear interpolation, and call these default.
  default = .newx %>%
    dplyr::mutate(
      emissions = funs$emissions(year),
      vmt = funs$vmt(year),
      vehicles = funs$vehicles(year),
      starts = funs$starts(year),
      sourcehours = funs$sourcehours(year)
    )
  # For as many xvars are supplied, overwrite the xvar with the custom value
  custom = default
  # Void the emissions column, since we don't actually know that.
  custom$emissions = NA
  for(i in .xvars){ custom[[i]] <- .newx[[i]] }

  # Bind and output
  output = dplyr::bind_rows(
    default %>% dplyr::mutate(type = "benchmark"),
    custom %>% dplyr::mutate(type = "custom")
  )

  # If you want the full range of default information available, add it in to the output
  if(.context == TRUE){
    # Filter MOVES estimated data to only years that are not in custom year AND that are less than the min custom year
    past = d %>% dplyr::filter(!year %in% custom$year & year < min(custom$year) )
    # Filter MOVES estimated data to just all future years AFTER The minimum custom year and NOT in the default year data
    future = d %>% dplyr::filter(year >= min(custom$year), !year %in% default$year )

    output = dplyr::bind_rows(
      output,
      past %>% dplyr::mutate(type = "pre_benchmark"),
      future %>% dplyr::mutate(type = "post_benchmark")
    )
  }

  return(output)
}
