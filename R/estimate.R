#' @name estimate
#' @title Estimate your area-specific model
#' @author Tim Fraser & Yan Guo
#' @param .filters (vector/list) Named Vector or List of inputted values for filtering.
#' @param .vars (character vector) Vector of variables to use as predictors.
#' @param .best (logical) `TRUE` or `FALSE` - if TRUE, always tries to make the 'best' fitting model from moveslite paper.
#' @description using statistical model to estimate the function
#' @importFrom dplyr `%>%` filter tbl
#' @importFrom broom glance
#' @importFrom stats lm
#' @note An example filter would be: `.filters = c(.by = 8, .pollutant = 98, .sourcetype = 42)`
#' @export

estimate = function(data, .vars = c("vmt", "vehicles", "starts", "sourcehours", "year"), .best = TRUE){

  # -- Do a few versions of this model to account for different numbers of .vars
  if(.best == FALSE){
    # Make a vector to contain formula terms
    f = c()
    if("vmt" %in% .vars){ f = c(f, "poly(log(vmt),3)") }
    if("year" %in% .vars){ f = c(f, "poly(year,2)") }
    if("starts" %in% .vars){ f = c(f, "starts") }
    if("sourcehours" %in% .vars){ f = c(f, "sourcehours") }
    if("vehicles" %in% .vars){ f = c(f, "vehicles") }
    if("idlehours" %in% .vars){ f = c(f, "idlehours") }
    if("bus" %in% .vars){ f = c(f, "bus") }
    if("car_bike" %in% .vars){ f = c(f, "car_bike") }
    if("light_truck" %in% .vars){ f = c(f, "light_truck") }
    if("combo_truck" %in% .vars){ f = c(f, "combo_truck") }
    if("heavy_truck" %in% .vars){ f = c(f, "heavy_truck") }
    if("hhd8" %in% .vars){ f = c(f, "hhd8") }
    if("ldt" %in% .vars){ f = c(f, "ldt") }
    if("ldv" %in% .vars){ f = c(f, "ldv") }
    if("lhd34" %in% .vars){ f = c(f, "lhd34") }
    if("lhd45" %in% .vars){ f = c(f, "lhd45") }
    if("mc" %in% .vars){ f = c(f, "mc") }
    if("mhd67" %in% .vars){ f = c(f, "mhd67") }
    if("urban_bus" %in% .vars){ f = c(f, "urban_bus") }
    if("glider" %in% .vars){ f = c(f, "glider") }
    if("gas" %in% .vars){ f = c(f, "gas") }
    if("diesel" %in% .vars){ f = c(f, "diesel") }
    if("ethanol" %in% .vars){ f = c(f, "ethanol") }
    if("cng" %in% .vars){ f = c(f, "cng") }
    if("electric" %in% .vars){ f = c(f, "electric") }
    if("rural_restricted" %in% .vars){ f = c(f, "rural_restricted") }
    if("rural_unrestricted" %in% .vars){ f = c(f, "rural_unrestricted") }
    if("urban_restricted" %in% .vars){ f = c(f, "urban_restricted") }
    if("urban_unrestricted" %in% .vars){ f = c(f, "urban_unrestricted") }

    # Add in the outcome
    outcome = "log(emissions)"
    fhat = f %>% paste0(collapse = " + ") %>% paste0(outcome, " ~ ", .)

  }else if(.best == TRUE){
    # Use this best fitting model
    fhat = "log(emissions) ~ poly(log(vmt),3) + poly(year,2) + (vehicles) + (sourcehours) + starts"
  }

  # Convert character statement to formula
  formula = as.formula(fhat)

  # Compute the model
  m = data %>% stats::lm(formula = formula)

  # Return model.
  return(m)

}
