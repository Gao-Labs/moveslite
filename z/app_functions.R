#' @name get_default
#' @author Tim
#' @description
#' Produces `default()` data for a given CATSERVER query.
#'
#' @param .scenario ...
#' @param .pollutant ...
#' @param .by ...
#'
#' @export
get_default = function(.scenario = "granddata.d36109", .pollutant = 98, .by = "8.41"){
  # Testing values
  # input = list(scenario = "granddata.d36109", pollutant = 98, by = choices_aggregation[[2]][5])
  # input = list(scenario = "granddata.dXXXXX", pollutant = 98, by = choices_aggregation[[2]][5])

  # Get data
  # .scenario = input$scenario
  # .pollutant = input$pollutant
  # .by = input$by

  # Derive inputs
  .dbname = stringr::str_remove(.scenario, pattern = "[.].*")
  .table = stringr::str_remove(.scenario, pattern = paste0(.dbname, "[.]"))

  .byvalues = stringr::str_split(string = input$by, pattern = "[.]") %>% unlist()
  .byid = as.integer(.byvalues[[1]])
  # Build initial filter
  .filters = list(.pollutant = .pollutant, .by = .byid)
  # If there are 2 byvalues, grab the second; otherwise, leave it null
  if(length(.byvalues) > 1){

    .bytype = as.integer(.byvalues[[2]])
    if(.byid == 14){ .filters$.fueltype = .bytype }
    if(.byid == 8){ .filters$.sourcetype = .bytype }
    if(.byid == 15){ .filters$.roadtype = .bytype }
    if(.byid == 12){ .filters$.regclass = .bytype }
  }

  db = connect(.dbname)

  default = query_many(
    .db = db, .table = .table, .filters = .filters,
    .vars = c("vmt", "vehicles", "sourcehours", "starts")) %>%
    select(-any_of(c("geoid")))
  DBI::dbDisconnect(db)

  return(default)
}


get_vars = function(.default){
  .vars = names(.default);
  # Extract usable variables
  .vars = .vars[!.vars %in% c("geoid", "sourcetype", "roadtype", "fueltype", "regclass", "by", "emissions")]
  return(.vars)
}

#' @name find_stats()
#' @author Tim Fraser
#' @param m Model object
#' @param .unit typically input$unit
#' @description Function to calculate goodness of fit statistics
#' @importFrom scales percent number cut_si
#' @importFrom dplyr `%>%` select mutate if_else
#' @importFrom broom glance
find_stats = function(m, .unit = "tons"){

  # m = model()
  #.unit = "tons"

  stats = m %>%
    broom::glance() %>%
    # Extract quantities of interest
    select(accuracy = adj.r.squared, sigma, p_value = p.value, df, nobs) %>%
    # Format them
    mutate(
      # Get R2 as a number
      accuracygauge = round(accuracy*100, 1),
      p_value = round(p_value, 3),
      p_value = if_else(p_value == 0, true = "p < 0.001", false = paste0("p = ", p_value)))

  # Find transformation
  t = find_transformation(m)
  # Get year range
  #stats$year_lower = m$model$year %>% min(na.rm = TRUE)
  #stats$year_upper = m$model$year %>% max(na.rm = TRUE)

  # Get range of outcome variable
  ydata = tibble(y = m$model[, 1])

  # If there is a transformation
  if(t$trans != "asis"){
    # Backtransform it
    ydata = ydata %>% mutate(y = eval(parse(text = t$backtrans)))
    # Update stats sigma with a simulated version in the original units
    stats$sigma = tibble(y = rnorm(n = 1000, mean = mean(m$model[,1]), sd = stats$sigma)) %>%
      mutate(y = eval(parse(text = t$backtrans))) %>%
      summarize(sd = sd(y)) %>%
      with(sd)

  }

  stats$y_lower = ydata$y %>% min(na.rm = TRUE)
  stats$y_upper = ydata$y %>% max(na.rm = TRUE)

  stats = stats %>%
    mutate(#yearrange = paste0(year_lower, " - ", year_upper),
           nyears = paste0(nobs, " cases"),
           # Get range
           yrange = abs(y_upper - y_lower),
           # Get percentage of range that sigma takes up
           percentrange = sigma / yrange,
           percentrange = scales::percent(percentrange, accuracy = 0.1, suffix = "% of range"),
           sigma = scales::number(sigma, accuracy = 0.01, scale_cut = scales::cut_si(unit = .unit)) %>% paste0("±", .),

           yrange = scales::number(yrange, accuracy = 0.01, scale_cut = scales::cut_si(unit = .unit)),
           y_lower = scales::number(y_lower, accuracy = 0.01, scale_cut = scales::cut_si(unit = .unit)),
           y_upper = scales::number(y_upper, accuracy = 0.01, scale_cut = scales::cut_si(unit = .unit)),
           yinterval = paste0(y_lower, " - ", y_upper)
    )
  return(stats)
}


get_newx = function(.default, .data){
  # .default = default()
  # .data = data()
  n_rows = nrow(.default)
  n_cols = ncol(.default)
  # Make a matrix to hold results
  ids = matrix(data = NA, nrow = n_rows, ncol = n_cols)
  # For each row and column
  for(i in 1:n_rows){
    for(j in 1:n_cols){
      #i = 1; j = 1
      # If it DOESN'T match, return FALSE
      # If it DOES MATCH, return TRUE
      ids[i,j] <- .default[[i,j]] != .data[[i,j]]
    }
  }
  # Count up total 'TRUEs' (don't match) per row
  keep = rowSums(ids)

  # Get the row indices that should be kept
  indices = tibble(row = 1:length(keep),
         keep = keep > 0) %>%
    filter(keep == TRUE) %>%
    with(row)

  # Filter the customized data.frame to just the edited values
  newx = .data[indices, ]
  # Return
  return(newx)
}

#' @name get_ydata
#' @importFrom dplyr `%>%` left_join select mutate
get_ydata = function(.model, .default, .newx, .cats = "year", .context = TRUE, .ci = 0.95){

  y = project(m = .model, data = .default, .newx = .newx, .cats = "year", .context = TRUE, .ci = 0.95)

  result = y %>%
    left_join(
      by = "year",
      y = .default %>% select(year, default = emissions)) %>%
    mutate(change = emissions - default)

  return(result)
}


#' @name qi_scenario
#' @description
#' Generate quantities of interest necessary for visualizing with `h_scenario()`
qi_scenario = function(.ydata,
                       .pollutant = 98,
                       .unit = "t",
                       .scenario_benchmark = "Benchmark Scenario",
                       .scenario_yours = "Your Scenario",
                       .startyear = 2020
                       ){

  # Testing Values
  # .ydata = ydata()
  # .pollutant = 98; .unit = "t";   .startyear = 2020;
  # .scenario_benchmark = "Benchmark Scenario"; .scenario_yours = "Your Scenario"


  .scenario_name_benchmark = .scenario_benchmark
  .scenario_name_yours = .scenario_yours

  # .geoid_label = readr::read_rds("data/calculator/areas.rds") %>% filter(geoid == input$geoid) %>% with(label)
  .geoid_label = "GEOID LABEL HERE"

  # Load Keywords
  .pollutant_info = moveslite::keywords %>%
    filter(type == "pollutant") %>%
    filter(id == .pollutant)


  .pollutant_label = .pollutant_info$term
  .pollutant_unit = switch(EXPR = .unit, "t" = "tons")
  .pollutant_unit_abbr = stringr::str_sub(.pollutant_unit, 1, 1)
  .startyear = as.numeric(.startyear)
  #.startyear = 2023
  # Get the most recently supplied 5 year increment
  .prioryear = .startyear %>%
    paste0(., "-01-01") %>%
    lubridate::date() %>%
    lubridate::floor_date(unit = "5 years") %>%
    lubridate::year()
  .prioryear = if(.prioryear < 1990){ 1990 }else{ .prioryear }

  bridge = .ydata %>%
    filter(type == "pre_benchmark" & year == .prioryear) %>% mutate(type = "custom")


  ydata2 = .ydata %>%
    filter(type %in% c("custom", "benchmark", "pre_benchmark", "post_benchmark")) %>%
    # Join in the bridge year
    bind_rows(bridge) %>%
    # Mutate labels
    mutate(type = case_when(type == "custom" ~ "custom",
                            type != "custom" ~ "benchmark"),
           label_type = factor(type, levels = c("benchmark", "custom"),
                               labels = c(.scenario_name_benchmark,
                                          .scenario_name_yours))) %>%
    mutate(label_emissions = scales::number(emissions, accuracy = .1, scale_cut = scales::cut_si(unit = paste0(" ", .pollutant_unit)))) %>%
    mutate(text = paste0(
      "<b>Type</b>: ", label_type,
      "<br>",
      "<b>Year</b>: ", year,
      "<br>",
      "<b>Emissions</b>: ", label_emissions))


  data = ydata2 %>% filter(type %in% c("benchmark", "custom"))
  # Get the bridge from the previous year

  # Example
  # tibble(type = c("benchmark", "custom"),
  #        year = c(2022, 2022),
  #        emissions = c(345, 263))%>%
  #   select(type, year, emissions) %>%
  #   pivot_wider(id_cols = c(year), names_from = type, values_from = emissions) %>%
  #   mutate(change = custom - benchmark)
  #
  gaps = data %>%
    select(type, year, emissions) %>%
    pivot_wider(id_cols = c(year), names_from = type, values_from = emissions) %>%
    # Compute Quantities
    mutate(change = custom - benchmark,
           percent = change / benchmark) %>%
    rowwise() %>%
    mutate(ymin = min(c(custom, benchmark), na.rm = TRUE),
           ymax = max(c(custom, benchmark), na.rm = TRUE)) %>%
    ungroup() %>%
    # Generate Labels
    mutate(label_custom = scales::number(custom, accuracy = 0, scale_cut = scales::cut_si(unit = paste0(" ", .pollutant_unit))),
           label_benchmark = scales::number(benchmark, accuracy = 0, scale_cut = scales::cut_si(unit = paste0(" ", .pollutant_unit))),
           label_change = scales::number(change, accuracy = 0, style_positive = "plus", style_negative = "minus", scale_cut = scales::cut_si(unit = paste0(" ", .pollutant_unit))),
           label_percent = scales::percent(percent, style_positive = "plus", style_negative = "minus", suffix = "%")
    ) %>%
    mutate(text = paste0(
      "<b>Year</b>: ", year,
      "<br>",
      "<b>", .scenario_name_benchmark, "</b>:", label_benchmark,
      "<br>",
      "<b>", .scenario_name_yours, "</b>:", label_custom,
      "<br>",
      "<b>Change</b>:", label_change, " (", label_percent, ")"))

  # Compile all as a single output list
  output = list(
    gaps = gaps,
    data = data,
    .scenario_name_benchmark = .scenario_name_benchmark,
    .scenario_name_yours = .scenario_name_yours,
    .pollutant_unit_abbr = .pollutant_unit_abbr,
    .pollutant_unit = .pollutant_unit,
    .pollutant_label = .pollutant_label
  )

  return(output)
}


#' @name h_scenario()
#'
#' @description
#' Visualize Mulitple Scenarios as a line plot
h_scenario = function(data){

  require(ggplot2)
  require(dplyr)
  require(plotly)
  require(scales)

  .highlight_color = "#2fa4e7"
  .scenario_name_benchmark = data$.scenario_name_benchmark
  .scenario_name_yours = data$.scenario_name_yours

  benchmark = data$data %>% filter(type == "benchmark")
  custom = data$data %>% filter(type == "custom")

  gg = ggplot() +
    geom_line(
      data = benchmark,
      mapping = aes(x = year, y = emissions, group = type, color = type, text = text),
      linewidth = 1.5) %>%
    suppressWarnings() +
    geom_line(
      data = custom,
      mapping = aes(x = year, y= emissions, group = type, color = type, text = text),
      linewidth = 1.5) %>%
    suppressWarnings() +
    geom_linerange(
      data = data$gaps,
      mapping = aes(x = year, ymin = ymin, ymax = ymax, text = text),
      linewidth = 1.15) %>%
    suppressWarnings() +
    geom_point(
      data = benchmark,
      mapping = aes(x = year, y= emissions, color = type, text = text),
      shape = 21, size = 3, fill = "white", stroke = 0.75) %>%
    suppressWarnings() +
    geom_point(
      data = custom,
      mapping = aes(x = year, y= emissions, color = type, text = text),
      shape = 21, size = 3, fill = "white", stroke = 0.75)  %>%
    suppressWarnings() +
    scale_color_manual(
      breaks = c(.scenario_name_benchmark = "benchmark", .scenario_name_yours = "custom"),
      name = "Scenario",
      guide = "none",
      values = c("grey", .highlight_color)) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si(data$.pollutant_unit_abbr))) +
    labs(y = paste0("Estimated Emissions\n(", data$.pollutant_unit, " of ", data$.pollutant_label, ")"),
         x = "Year",
         title = paste0("Projected Emissions over Time in ", data$.pollutant_label)) +
    theme(legend.position = "none")

  pp = ggplotly( gg, tooltip = "text")

  # Update configuration
  pp = pp %>%
    layout(hoverlabel = list(align = "left")) %>%
    config(displayModeBar = FALSE, displaylogo = FALSE)

  return(pp)
}



