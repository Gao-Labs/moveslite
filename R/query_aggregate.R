#' @name query_aggregate()
#' @description Aggregate/relabel a queried subset, to generate mode-mix / fuel-mix
#'
#' @param .db database connection object for CAT GRAND database or CAT formatted MOVES output data
#' @param .table Name of table in database object `.db`
#' @param .by Aggregation level ID to filter to. (overall = `16`, by sourcetype = `8`, by fueltype = `14`, by regulatory class = `12`)
#' @param .pollutant ID of the pollutant affected
#'
#' @importFrom purrr set_names
#' @importFrom stringr str_replace_all
#' @importFrom readr read_csv
#' @importFrom dplyr recode tribble %>% mutate filter summarize group_by ungroup any_of
#' @importFrom tidyr pivot_wider
#' @export

query_aggregate = function(data, .by = 8){

  traits = tribble(
    ~id, ~name,        ~var,
    16,  "overall", 'vehicles',
    8, "sourcetype", 'vehicles',
    14, "fueltype", 'vehicles',
    15, "roadtype", 'vmt',
    12, "regclass", 'vehicles')

  # Get your traits
  .traits = traits %>% filter(id == .by)

  if(.traits$id != 16){
    # Get these variables BY AGGREGATION LEVEL
    dagg = data %>%
      filter(by %in% .traits$id) %>%
      select(any_of(c("geoid", "year", "emissions", .traits$name, .traits$var)))

    # Gather variable names
    key = read_csv("diagnostics/keywords.csv") %>%
      filter(type %in% .traits$name) %>%
      select(id, label) %>%
      mutate(label = stringr::str_replace_all(label, "[\n]|[/]|[ ]+", "_") %>% stringr::str_replace_all("[_]+", "_") %>% tolower()) %>%
      with(purrr::set_names(.$label, .$id)) %>%
      as.list()

    # Aggregate these extra variables if necessary.
    dextra = dagg %>%
      # Recode variables
      mutate(type = !!sym(.traits$name) %>% dplyr::recode( !!!key)) %>%
      # Aggregate
      group_by(year, geoid, type) %>%
      summarize(across(.cols = any_of(.traits$var),
                       .fns = ~sum(.x, na.rm = TRUE))) %>%
      ungroup() %>%
      # Convert to percentage
      group_by(year, geoid) %>%
      mutate(across(.cols = any_of(.traits$var),
                    .fns = ~.x / sum(.x, na.rm = TRUE))) %>%
      # Pivot
      tidyr::pivot_wider(id_cols = c(year, geoid), names_from = "type", values_from = .traits$var, values_fill = purrr::set_names(x = 0, nm = .traits$var))



    return(dextra)

  }else{ print("Error: this function was not built to serve by = 16. Use query() instead.") }
}
