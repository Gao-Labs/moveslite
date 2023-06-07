#' @name estimate()
#' @title Estimate your area-specific model
#' @param .filters (vector/list) Named Vector or List of inputted values for filtering.
#' @param .vars (character vector) Vector of variables to use as predictors.
#' @importFrom dplyr %>% filter tbl
#' @importFrom broom glance
#' @importFrom stats lm
#' @note An example filter would be: `.filters = c(.by = 8, .pollutant = 98, .sourcetype = 42)`
#' @export

estimate = function(data, .vars = c("vmt", "vehicles", "starts", "sourcehours", "year")){

  ####################################
  # EDITS NEEDED
  ###################################
  # -- Do a few versions of this model to account for different numbers of .vars

  # Ideas:
  # .type

  # 10 models that make plausible sense that try to maximize R2

  # switch(
  #   EXPR = .type,
  #
  #   "1" = { m = data %>% lm(formula = emissions ~ vmt + vehicles + starts + sourcehours + year)   },
  #
  #   "2" = { m = data %>% lm(formula = emissions ~ poly(vmt, 2) + vehicles + starts + sourcehours + year)  },
  #
  #   "3" = { m = data %>% lm(formula = emissions ~ poly(vmt, 3) + vehicles + starts + sourcehours + year)  },
  #
  #   "4" = {  m = data %>% lm(formula = emissions ~ vmt + year) }
  #   )

  # Use this best fitting model
  formula = emissions ~ poly(vmt, 2) + poly(vehicles,2) + poly(sourcehours, 2) + poly(starts,2) + year

  # Compute the model
  m = data %>% lm(formula = formula)

  # Return model.
  return(m)

}
