#' @name effect()
#' 
#' @description Calculate marginal effect of a change in inputs.
#' @param .setx a data.frame of predictor values
#' 

effect = function(data){

  .vars = names(data)
  
  .mainvars = .vars[!.vars %in% c("year", "type")]
  
  qi = stat %>% 
    filter(type %in% c("benchmark", "custom")) %>%
    # For each year, measure the change in variables
    group_by(year) %>%
    summarize(
      across(
        .cols = any_of(.mainvars), 
        .fns = ~.x[type == "custom"] - .x[type == "benchmark"]), 
      .groups = "drop")
  
  return(qi)
  
}