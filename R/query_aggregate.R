#' @name query_aggregate
#' @title query_aggregate
#' @description Aggregate/relabel a queried subset, to generate mode-mix / fuel-mix
#' @author Tim Fraser & Yan Guo
#' @param .db database connection object for CAT GRAND database or CAT formatted MOVES output data
#' @param .table Name of table in database object `.db`
#' @param .by Aggregation level ID to filter to. (overall = `16`, by sourcetype = `8`, by fueltype = `14`, by regulatory class = `12`)
#' @param .pollutant ID of the pollutant affected
#' @importFrom purrr set_names
#' @importFrom stringr str_replace_all
#' @importFrom readr read_csv
#' @importFrom dplyr `%>%` recode tribble mutate filter summarize group_by ungroup any_of across
#' @importFrom tidyr pivot_wider
#' @importFrom base tolower
#' @export

query_aggregate = function(data, .by = 8){

  traits = dplyr::tribble(
    ~id, ~name,        ~var,
    16,  "overall", 'vehicles',
    8, "sourcetype", 'vehicles',
    14, "fueltype", 'vehicles',
    15, "roadtype", 'vmt',
    12, "regclass", 'vehicles')

  # Get your traits
  .traits = traits %>% dplyr::filter(id == .by)

  if(.traits$id != 16){
    # Get these variables BY AGGREGATION LEVEL
    dagg = data %>%
      dplyr::filter(by %in% .traits$id) %>%
      dplyr::select(dplyr::any_of(c("geoid", "year", "emissions", .traits$name, .traits$var)))

    # Gather variable names
    key = keywords %>%
      dplyr::filter(type %in% .traits$name) %>%
      dplyr::select(id, label) %>%
      dplyr::mutate(label = stringr::str_replace_all(label, "[\n]|[/]|[ ]+", "_") %>% stringr::str_replace_all("[_]+", "_") %>% base::tolower()) %>%
      with(purrr::set_names(.$label, .$id)) %>%
      as.list()

    # Aggregate these extra variables if necessary.
    dextra = dagg %>%
      # Recode variables
      dplyr::mutate(type = !!sym(.traits$name) %>% dplyr::recode( !!!key)) %>%
      # Aggregate
      dplyr::group_by(year, geoid, type) %>%
      dplyr::summarize(dplyr::across(.cols = dplyr::any_of(.traits$var),
                       .fns = ~sum(.x, na.rm = TRUE))) %>%
      dplyr::ungroup() %>%
      # Convert to percentage
      dplyr::group_by(year, geoid) %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::any_of(.traits$var),
                    .fns = ~.x / sum(.x, na.rm = TRUE))) %>%
      # Pivot
      tidyr::pivot_wider(id_cols = c(year, geoid), names_from = "type", values_from = .traits$var, values_fill = purrr::set_names(x = 0, nm = .traits$var))



    return(dextra)

  }else{ print("Error: this function was not built to serve by = 16. Use query() instead.") }
}
