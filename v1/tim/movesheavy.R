#' @name workflow.R
#' @title Example Workflow for `moveslite`
#' @author Tim Fraser
#' @description A demo of the intended workflow for a `moveslite` user.


#' @note Packages you need to load!
library(dplyr)
library(broom)
library(readr)
library(tidyr)
library(stringr)

#' These are the 5 core functions in `moveslite`.
source("R/connect.R")    # connect() to a database
source("R/query.R")      # query() that database in a specific way
source("R/query_aggregate.R")# query_aggregate() - query a special extra subset
source("R/query_many.R")# query_many() - wrapper for making complex queries
source("R/setx.R")       # setx() - create newdata from default data to feed to predict()
source("R/estimate.R")   # estimate() a model of the default data
source("R/project.R")    # generate predictions with project()


unloadNamespace("moveslite"); remove.packages("moveslite")
warnings()
devtools::load_all(".")
devtools::document()

#' Our goal is to extend their functionality.

#' Here's an example of their usage.

# Initiate the 'anydata' connection
library(dplyr)
library(DBI)


db %>% db_list_tables()

#dbGetQuery(conn = db, statement = "SELECT * FROM granddata.d36109 LIMIT 3;")
#tbl(db, "granddata.36109")


# Connect to the 'data' database (tenatively your z/db.sqlite file)
#db = connect("data")

# Total CAT Format is about n = 9000
# by = 16 = Overall
# by = 8 = Sourcetype
# by = 12 = Regulatory Class
# by = 14 = Fueltype
# by = 15 = Roadtype
moveslite::keywords

.scenario = "granddata.d36109"
.dbname = str_remove(.scenario, pattern = "[.].*")
.table = str_remove(.scenario, pattern = paste0(.dbname, "[.]"))

db = connect("granddata")

query_many(
  db, .table = "d36109",
  .filters = list(.pollutant = 98, .by = 8),
  .vars = c("sourcetype", "emissions"))

query_many(
  db, .table = "d36109",
  .filters = list(.pollutant = 98, .by = 8, .sourcetype = 31),
  .vars = c("sourcetype", "emissions"))

query_many(
  db, .table = "d36109",
  .filters = list(.pollutant = 98, .by = 8, .sourcetype = c(21,31)),
  .vars = c("sourcetype", "emissions"))

query_many(
  db, .table = "d36109", .filters = list(.pollutant = 98, .by = 17),
  .vars = c("emissions", "vehicles"))

query_many(
  db, .table = "d36109", .filters = list(.pollutant = 98, .by = 20),
  .vars = c("emissions", "vmt"))

# This doesn't make sense - there are 2 bys here
# query_many(
#   db, .table = "d36109", .filters = list(.pollutant = 98, .by = c(16, 8)),
#   .vars = c("emissions", "vehicles", "vmt"))

# Simplify categories


data %>%
  lm(formula = emissions ~ year + poly(vmt, 2) + bus + car_bike + combo_truck - 1) %>%
  summary()

dbDisconnect(db); rm(list = ls()); gc()
# Idea:

# GEOID

# PROCESS: MOVESLiter vs. MOVESHeavier

# AGGREGATION

# TYPE

# SUBTYPE

# MODEL: 1, 2, or 3








#' @name find_stats()
#' @author Tim Fraser
#' @param m Model object
#' @param .unit typically input$unit
#' @description Function to calculate goodness of fit statistics

find_stats = function(m, .unit){

  #unit = input$unit

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

  # Get year range
  stats$year_lower = m$model$year %>% min(na.rm = TRUE)
  stats$year_upper = m$model$year %>% max(na.rm = TRUE)
  # Get range of outcome variable
  stats$y_lower = m$model$emissions %>% min(na.rm = TRUE)
  stats$y_upper = m$model$emissions %>% max(na.rm = TRUE)

  stats = stats %>%
    mutate(yearrange = paste0(year_lower, " - ", year_upper),
           nyears = paste0(nobs, " cases"),
           # Get range
           yrange = abs(y_upper - y_lower),
           # Get percentage of range that sigma takes up
           percentrange = sigma / yrange,
           percentrange = percent(percentrange, accuracy = 0.1, suffix = "% of range"),
           sigma = number(sigma, accuracy = 0.01, scale_cut = cut_si(unit = .unit)) %>% paste0("±", .),

           yrange = number(yrange, accuracy = 0.01, scale_cut = cut_si(unit = .unit)),
           y_lower = number(y_lower, accuracy = 0.01, scale_cut = cut_si(unit = .unit)),
           y_upper = number(y_upper, accuracy = 0.01, scale_cut = cut_si(unit = .unit)),
           yinterval = paste0(y_lower, " - ", y_upper)
    )
  return(stats)
}


#' @name print_equation()
#' @author Tim Fraser
#' @param m Model object outputted from `estimate()`

print_equation = function(m){
  # This is the ibm color scale
  ibm = list(
    "blue" = "#648FFF",
    "purple" = "#785EF0",
    "red" = "#DC267F",
    "orange" = "#FE6100",
    "yellow" =  "#FFB000",
    "grey" = "#373737")

  # Labels for any possible version
  v = tribble(
    ~var,   ~label,   ~color,
    # Basic Covariates
    "year", "Year", ibm$grey,
    # Fleet
    "vehicles", "Vehicles", ibm$blue,
    "vehicles:year", "Vehicles x Year", ibm$blue,
    "poly(vehicles,2)", "Vehicles", ibm$blue,
    "poly(vehicles,3)", "Vehicles", ibm$blue,
    "poly(vehicles, 2)", "Vehicles", ibm$blue,
    "poly(vehicles, 3)", "Vehicles", ibm$blue,

    # Activity
    "vmt",  "VMT", ibm$orange,
    "vmt:year", "VMT x Year", ibm$orange,
    "poly(vmt,2)",  "VMT", ibm$orange,
    "poly(vmt,3)",  "VMT", ibm$orange,
    "poly(vmt, 2)",  "VMT", ibm$orange,
    "poly(vmt, 3)",  "VMT", ibm$orange,

    "sourcehours", "Time Driven", ibm$orange,
    "sourcehours:year", "Time Driven x Year", ibm$orange,
    "poly(sourcehours,2)",  "VMT", ibm$orange,
    "poly(sourcehours,3)",  "VMT", ibm$orange,
    "poly(sourcehours, 2)",  "VMT", ibm$orange,
    "poly(sourcehours, 3)",  "VMT", ibm$orange,

    "starts", "Starts", ibm$orange,
    "starts:year", "Starts x Year", ibm$orange,
    "poly(starts,2)",  "Starts", ibm$orange,
    "poly(starts,4)",  "Starts", ibm$orange,
    "poly(starts, 2)",  "Starts", ibm$orange,
    "poly(starts, 4)",  "Starts", ibm$orange,

    # Outcome
    "emissions", "Emissions", ibm$red)



  # Gather appropriate labels; it will eject any that don't apply automatically.
  var_names = set_names(v$label, v$var)
  var_colors = set_names(v$color, v$var)

  result = m %>%
    extract_eq(
      use_coefs = TRUE, fix_signs = TRUE,
      wrap = TRUE,
      swap_var_names = var_names,
      var_colors = var_colors,
      coef_digits = 2) %>%
    withMathJax()

  return(result)
}


#' @name print_note()
#' @author Tim Fraser
#' @param m model object from estimate(), with a $yourformula field
#' @param scaling typically input$scaling
#' @param unit typically input$unit

print_note = function(yourformula, scaling, unit){
  # Get scale
  xscale = switch(
    EXPR = scaling,
    "1" = "",
    "1000" = "thousand (k)",
    "1e+06" = "million (M)",
    "1e+09" = "billion (G)"
  )

  yscale = switch(
    EXPR = unit,
    "t" = "",
    "kilotons" = "thousand (k)",
    "megatons" = "million (M)",
    "gigatons" = "billion (G)")

  # Load formulas details
  .vars = yourformula$vars %>% unlist()

  # Recode them into labels
  .labels = .vars %>% dplyr::recode_factor(
    "emissions" = paste0("Emissions: per ", yscale, " tons"),
    "vmt" = paste0("VMT: per ", xscale, " miles"),
    "vehicles" = paste0("Vehicles: per ", xscale, " vehicles"),
    "sourcehours" = paste0("Time Driven: per ", xscale, " hours"),
    "starts" = paste0("Vehicle Starts: per ", xscale, " times")
  )
  # Create the message:
  message = paste0("Units: ", paste0(.labels, collapse = "; ") )
  # Print the message
  return(message)

}

#' @name query_download()
#' @description
#' Based on data for one pollutant and geoid provided from a `query()`,
#' download just that data. Wrapper for `query()`, used in shinyapp.
#'
#' @importFrom dplyr tribble mutate filter left_join select any_of
#' @export

query_download = function(.db, .table = "d36109", .pollutant = 98,
                          .vars = c("vmt", "vehicles", "sourcehours", "starts")){

  # Inputs for testing only
  # source("R/connect.R")
  # source("R/query.R")
  # source("R/query_aggregate.R")
  # db = connect("data")
  # .by = 18
  # .pollutant = 98
  # .table = "d36109"
  # .vars = c("vmt", "vehicles", "sourcehours", "starts")
  load("data/moveslite/query.rda")

  # Get these variables at EACH LEVEL OF AGGREAGTION NEEDED
  data = query(
    .db = .db, .table = .table,
    .filters = list(.pollutant = .pollutant),
    .vars = c("by", "sourcetype", "fueltype", "regclass", 'roadtype',
              "emisions", .vars))

  return(data)
}

#' @name convert()
#' @author Tim Fraser
#' @description Function to convert outcome predictions and standard error back from a transformation, using simulation.
#' @importFrom dplyr %>% tibble
#' @importFrom stringr str_detect
#' @importFrom stats rnorm sd quantile

convert = function(y, se, backtrans, df, ci = 0.95){
  # Execute the backtransformation on the original single estimate
  y_backtransformed = backtrans %>% parse(text = .) %>% eval()

  # Get 1000 simulated values
  # normally distributed around the original prediction y,
  # with a standard deviation of sigma
  # (Must call the output vector y, so that the parsing expression exp(y), etc. works on it)

  # If sample size were really large, we could do this
  # y = rnorm(n = 1000, mean = y, sd = se)

  # Since sample size is pretty small, we should do this.
  y = y + rt(n = 1000, df = df) * se

  # Now compute the backtransformation on the vector, producing ydist, in original units
  y_dist_backtransformed = backtrans %>% parse(text = .) %>% eval()

  # Calculate alpha level (eg. for 95% CI, alpha = 0.05)
  alpha = 1 - ci
  lower_ci = alpha / 2
  upper_ci = 1 - (alpha / 2)

  output = tibble(
    emissions = y_backtransformed,
    se = y_dist_backtransformed %>% sd(na.rm = TRUE),
    lower = y_dist_backtransformed %>% quantile(probs = lower_ci),
    upper = y_dist_backtransformed %>% quantile(probs = upper_ci),
  )

  return(output)
}


qi_scenario = function(input, ydata){

  .scenario_name_benchmark = input$scenario_benchmark
  .scenario_name_yours = input$scenario_yours

  .geoid_label = readr::read_rds("data/calculator/areas.rds") %>% filter(geoid == input$geoid) %>% with(label)

  # Load Keywords
  load("data/moveslite/keywords.rda")
  .pollutant_info = keywords %>%
    filter(type == "pollutant") %>%
    filter(id == input$pollutant)


  .pollutant_label = .pollutant_info$term
  .pollutant_unit = switch(EXPR = input$unit, "t" = "tons")
  .pollutant_unit_abbr = stringr::str_sub(.pollutant_unit, 1, 1)
  .startyear = as.numeric(isolate({input$startyear}))
  #.startyear = 2023
  # Get the most recently supplied 5 year increment
  .prioryear = .startyear %>%
    paste0(., "-01-01") %>%
    lubridate::date() %>%
    lubridate::floor_date(unit = "5 years") %>%
    lubridate::year()
  .prioryear = if(.prioryear < 1990){ 1990 }else{ .prioryear }

  bridge = ydata %>%
    filter(type == "pre_benchmark" & year == .prioryear) %>% mutate(type = "custom")


  .ydata = ydata %>%
    filter(type %in% c("custom", "benchmark", "pre_benchmark", "post_benchmark")) %>%
    # Join in the bridge year
    bind_rows(bridge) %>%
    # Mutate labels
    mutate(type = case_when(type == "custom" ~ "custom",
                            type != "custom" ~ "benchmark"),
           label_type = factor(type, levels = c("benchmark", "custom"),
                               labels = c(.scenario_name_benchmark, .scenario_name_yours))) %>%
    mutate(label_emissions = scales::number(emissions, accuracy = .1, scale_cut = scales::cut_si(unit = paste0(" ", .pollutant_unit)))) %>%
    mutate(text = paste0(
      "<b>Type</b>: ", label_type,
      "<br>",
      "<b>Year</b>: ", year,
      "<br>",
      "<b>Emissions</b>: ", label_emissions))


  data = .ydata %>% filter(type %in% c("benchmark", "custom"))
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
    scale_y_continuous(labels = scales::label_number(scale_cut = cut_si(data$.pollutant_unit_abbr))) +
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




mod_server_scenario = function(id = "", sets, model, default, default_yearly){
  moduleServer(
    id = id,
    module = function(input, output, session){

      load("data/calculator/ui_set.rda")
      load("data/calculator/column_wrap.rda")


      ## 4.1 ADD INPUT SET #####################################
      observe({
        # Require both before proceeding
        #req(default_yearly)
        req(input$add_set)
        # Update the counter
        sets$count <- sets$count + 1
        setcount <- sets$count
        # As long as there is more than 0 sets....
        # Fill in the values using default data
        #sets[[paste0("set", setcount)]]

        ns = NS(id)

        insertUI(
          selector = paste0("#", ns("inputsets")),
          where = "beforeBegin",
          ui = ui_set(id = id, setcount = setcount, data = default_yearly) )

      }) %>% bindEvent({input$add_set })

      ## 4.2 REMOVE INPUT SETS ################################
      observe({
        # If there is at least 1 input set active...
        if(sets$count > 0) {
          # Get the number of input sets
          setcount <- sets$count
          # Get the namespace
          ns <- NS(paste0(id, "set", "_", setcount))
          # Remove the input set
          removeUI(selector = paste0("#", ns("inputset")), multiple = TRUE)
          # Subtract 1 from the number of input sets
          sets$count <- sets$count - 1
        }
      }) %>% bindEvent({input$remove_set})

      ## 4.3 UPDATE PREDICTIONS ########################################
      ydata = reactive({
        #req( default, default_yearly, model)
        # Get the number of input sets
        setcount = sets$count
        # For these variables of interest
        vars = c("year", "vmt", "vehicles", 'sourcehours', "starts")

        # Print Setcount
        print(paste0("set: ", setcount))

        # Given at least 1 set of inputs...
        if(setcount > 0){
          # Don't continue until at least the first has shown up
          ns <- NS(paste0(id, "set", "_", 1))
          req(input[[ ns("year") ]])

          # For each set...
          xdata = 1:setcount %>%
            # Get xvar data
            map_dfr(.f = ~{
              # For these variables of interest
              vars = c("year", "vmt", "vehicles", 'sourcehours', "starts")
              # Get namespace of input set
              ns <- NS(paste0(id, "set", "_", .x))
              # For as many variables as desired...
              # Extract columns of these variables
              vars %>%
                # Bundle each into a tibble with proper names
                map(.f = ~{ tibble( input[[ ns(.x) ]] %>% as.numeric() ) %>% set_names(., nm = .x) }) %>%
                # Keep just valid, non-empty data.frames
                keep(~nrow(.) > 0) %>%
                # And bind the columns together!
                bind_cols()
            })

          # Generate statistics
          ydata = project(
            m = model, data = default, .newx = xdata,
            .cats = "year", .exclude = "geoid", .context = TRUE)

          # Calculate change in emissions from default
          ydata = ydata %>%
            left_join(
              by = "year",
              y = default_yearly %>% select(year, default = emissions)) %>%
            mutate(change = emissions - default)

          #print(head(ydata))

          # Whenever xdata() changes, update the predictions
          print("---ydata()"); return(ydata)
        }
      })


      ## 4.4. RENDER OUTPUT VALUES ###################################################################
      observe({
        req(default, default_yearly, ydata())
        setcount = sets$count
        # eg. setcount = 3
        if(setcount > 0){

          # Get the names of any input variables present
          vars = ydata() %>% select(any_of(c("year", "vmt", "vehicles", "sourcehours", "starts"))) %>% names()
          # Get exact slice of n (setcount) rows of data
          customdata = reactive({ ydata() %>% filter(type == "custom") %>% arrange(year) %>% select(emissions, change) })
          # Let's generate a reactive value, containing all the inputs from that set.
          lapply(X = 1:nrow(customdata()), FUN = function(i){
            # Get reactive vector of names
            input_names = reactive({ names(input)[ stringr::str_detect(names(input), pattern = ns(paste0("set_", i))) ] })
            # Assign the output
            ns = NS(id)
            # Print any input names
            print(input_names())

            output[[ ns(paste0("set_", i, "-emissions")) ]] <- renderText({ customdata()$emissions[i] }) %>% bindEvent(input, { input_names() })
            output[[ ns(paste0("set_", i, "-change")) ]] <- renderText({ customdata()$change[i] }) %>% bindEvent(input, { input_names() })
          })

        }
      }) %>% bindEvent({ydata()})

      x = reactiveValues()
      observe({ x$ydata = ydata() }) %>% bindEvent({ydata()})
      # Return ydata
      return(x$ydata)

    }
  )
}
mod_ui_scenario = function(id = ""){

  ns = NS(id)

  ui_scenarios = fluidRow(
    div(id = ns("inputsets")),
    # BUTTONS ################################################
    card(
      style = "border-color: transparent; padding: 2px; margin-bottom: 2px;",
      card_body(
        style = "padding: 5px; margin: 0px;",
        actionButton(inputId = ns("add_set"), icon = icon("circle-plus"), label =  "Add Input Set"),
        actionButton(inputId = ns("remove_set"), icon = icon("circle-minus"), label =  "Remove Input Set"))
    )
  )


}

#' @name ui_set()
#' @description Function to generate a set of text inputs and output boxes
#' @param setcount index number of the set to be produced
#' @param data data.frame of default annual interpolated data points to be entered as default inputs.
# Let's write a ui function to generate an input set
ui_set <- function(id = "", setcount, data, vars, scaling = 1e3){

  load("data/calculator/ui_card.rda")
  load("data/calculator/column_wrap.rda")

  # Get namespace
  ns <- NS(paste0(id, "set", "_", setcount))

  # Get the year of that setcount
  .year = data$year[setcount]

  # Get default data pertaining to that year
  .data = data %>% filter(year == .year)

  # Get available predictor variable names
  .vars = vars[!vars %in% c("emissions")]
  #.vars = .data %>% select(-any_of(c("emissions", "geoid"))) %>% names()
  # .vars = c("year", "vmt", "vehicles", "sourcehours", "starts")


  x_unit = switch(
    EXPR = scaling,
    "1" = "",
    "1000" = "k",
    "1e+06" = "M",
    "1e+09" = "G"
  )



  # Get labels
  labeldata = tribble(
    ~var, ~label,                ~unit,       ~width,
    "year", "Year",             "",           75,
    "vmt", "Miles Driven",    "",      125,
    "vehicles", "Vehicles",     "",           125,
    "sourcehours", "Hours Driven", "",    125,
    "starts", "Starts",          "",          125,


    # roadtype
    "urban_restricted", "Urban Restricted", "%", 125,
    "urban_unrestricted", "Urban Unrestricted", "%", 125,
    "rural_restricted", "Rural Restricted", "%", 125,
    "rural_unrestricted", "Rural Unrestricted", "%", 125,
    # fueltype
    "cng", "CNG", "%", 125,
    "diesel", "Diesel", "%", 125,
    "gas", "Gasoline", "%", 125,
    "ethanol", "Ethanol", "%", 125,
    # regulatory class
    "hhd8", "HHD8", "%", 125,
    "ldt", "LDT","%", 125,
    "ldv", "LDV","%", 125,
    "lhd34", "LHD34","%", 125,
    "lhd45", "LHD45","%", 125,
    "mc", "MC","%", 125,
    "mhd67", "MHD67","%", 125,
    "urban_bus", "Urban Bus","%", 125,
    "glider", "Glider","%", 125,
    # sourcetype
    "bus", "Buses","%", 125,
    "cars_bikes", "Cars/Bikes","%", 125,
    "combo_truck", "Combo Trucks","%", 125,
    "heavy_truck", "Heavy Trucks","%", 125,
    "light_truck", "Light Trucks", "%", 125,

    "emissions", "Emissions",    "tons",      125,
    "change", "Change",          "tons",       125
  ) %>%
    mutate(label = case_when(
      #var %in% c("") ~ paste0(label, " (", x_unit, " ", unit, ")"),
      unit == "%" ~ paste0(label, " (", unit, ")"),
      var %in% c("vehicles",  "vmt", "sourcehours", "starts") ~ paste0(label, " (", x_unit, ")"),
      var %in% c("emissions", "change") ~ paste0(label, " (", unit, ")"),
      var == "year" ~ label,
      TRUE ~ label)) %>%
    mutate(var = factor(var, levels = var),
           width = paste0(width, "px"))


  # Make an input text box for each predictor variable available
  bundle_inputs = labeldata %>%
    filter(var %in% .vars) %>%
    split(.$var, drop = TRUE) %>%
    map(~{
      # If it's not the first label, just make them blank
      if(setcount > 1){ mylabel = NULL }else{ mylabel = .x$label }

      ui_card(
        style = "padding-left: 5px; border-color: transparent;",
        textInput(inputId = ns(.x$var), label = mylabel, value = .data[[.x$var]], width = .x$width) )

    }
    )


  .outcomes = c("emissions", "change")

  # Make an output textbox for each outcome metric
  bundle_outputs = labeldata %>%
    filter(var %in% .outcomes) %>%
    split(.$var, drop = TRUE) %>%
    map(~{

      if(setcount > 1){ mylabel = NULL }else{ mylabel = tags$span(.x$label, class = "text-output-label") }

      ui_card(
        style = "padding-left: 15px; padding-right: 0px; overflow: hidden; border-color: transparent;",
        fluidRow(
          mylabel,
          div(
            class = "text-output",
            # Label each as, for example, set_1-emissions_1, set_2-emissions_2
            textOutput(outputId = ns(.x$var))) )  )

    }
    )

  # Combine them
  result = append(bundle_inputs, bundle_outputs) %>%
    column_wrap(
      l = ., width = 1/7,
      gap = "2px", style = "padding-left: 0px; padding-right: 0px;") %>%
    fluidRow(id = ns("inputset"), style = "padding-right: 0px;")

  return(result)
}

#' @name tooltip()
#' @description Write a short function to operate a tooltip
tooltip = function(.title, ...){
  htmltools::tags$span(
    `data-toggle` = "tooltip", `data-placement` = "right",
    title = .title, ...)
}


#' @name app.R
#' @title CAT Calculator Shiny App Source Code
#' @author Tim Fraser, PhD
#' @description A single file that integrates all supporting code. Can be excecuted using `shiny::runApp('calculator')`.







#' @name global()
#' @author Tim Fraser
#' @description Global setup function for ShinyApp

global = function(){

  # Load packages!
  shiny::shinyOptions(
    cache = cachem::cache_disk(
      Sys.getenv("PATH_CACHE_CALCULATOR"),
      max_size = 1e9))
  options(shiny.sanitize.errors = FALSE)

  load("data/calculator/packages.rda"); packages(); remove(packages)

  # Set ggplot theme for all visuals
  load("data/calculator/set_theme.rda"); set_theme(); remove(set_theme)
}




#' @name ui()
#' @author Tim Fraser
#' @description User Interface function for Calculator ShinyApp

ui = function(){

  # Load set theme
  load("data/calculator/ui_card.rda")
  load("data/calculator/column_wrap.rda")
  load("data/calculator/tooltip.rda")

  mycss = "
  .control-label {
    margin-bottom: 5px;  /* Adjust the margin value as per your needs */
  }
  .well {
    margin-right: 5px;
  }

  body {
    font-size: 14px;
  }
  .shiny-bound-input, .shiny-output-container {
    font-size: 14px;  /* Adjust the font size for input/output elements */
  }

  .form-label, .shiny-input-container .control-label {
    margin-bottom: 0.05rem;
    padding-bottom: 0px;
  }

  /* TEXT OUTPUT BOXES FOR EMISSIONS & CHANGE */
  .text-output-label {
    margin-left: 5px;
    padding: 0px;
  }
  .text-output {
    border: 1px solid #ced4da;
    background-color: #f5f5f5;
    border-radius: 4px;
    padding: 6px 12px;
    margin-top: 5px;
    margin-left: 2px;
    margin-right: 2px;
    margin-bottom: 5px;
    overflow: none;
    width: 100px;
    }
  .custom-sidebar {
    width: 225px;
    min-width: 175px;
    max-width: 250px;
    margin-right: 5px;
  }

  /* TOOLTIP DESIGN */
  .tooltip {
    pointer-events: none;
  }
  .tooltip > .tooltip-inner {
    pointer-events: none;
    background-color: #2fa4e7;
      color: #FFFFFF;
      border: 1px solid #2fa4e790;
    padding: 10px;
    font-size: 14px;
    text-align: left;
    margin-left: 0;
    max-width: 250px;
  }
  .tooltip > .arrow::before {
    border-right-color: #2fa4e7;
  }

  /* GAUGE DESIGN */
  .html-widget.gauge svg {
    height: 100%;
    width: 100%;
    margin-top: 0px; margin-bottom: 0px; margin-left: 0px; margin-right: 0px;
  }

  #toolbar {
    max-height: 100%;
    position: relative;
  }

  .selectize-dropdown {position: static; overflow: visible; }


  " %>%
    HTML()

  js = '$(function() {
        $("[data-toggle=\'tooltip\']").tooltip({
          html: true,
          container: "body",
          sanitize: false
        });
      });'


  # Build Title Panel ###################
  ui_title = titlePanel(
    title = card(
      style = css(
        `text-align` = "left",
        display = "flex",
        `justify-content` = 'space-between',
        `align-items` =  'baseline' ,
        `border-color` = "transparent",
        `margin-bottom` = "5px",
        `margin-top` = "2px"),
      column_wrap(
        style = "margins: 0px; overflow: hidden;",
        width = .50, style = css(grid_template_columns = "3fr 4fr"),
        l = list(
          card_title(tags$span("CAT CALCULATOR", icon("calculator")),
                     style = "font-size: 24px; text-align: left; margin-bottom: 0px; margin-top: 0px; padding: 0px; " ),
          card_body("powered by MOVESLite @ Cornell University",
                    style = "font-size: 14px; font-style: italic; text-align: right; vertical-align: bottom; overflow: hidden; border-color: transparent; margins: 0px; padding: 0px;")
        )),
      card_footer(style = "margin-top: 10px; text-align: left; font-size: 14px;", "Machine Learning Tools for Quick Emissions Reporting")
    ),
    windowTitle = "CAT Calculator"
  )


  # SIDEBAR PANEL ###################################
  ui_sidebar = sidebarPanel(
    width = 3,
    style = css(
      `margin-right` = "0px",
      `padding` = "0px",
      `max-width` = "100%",
      `min-width` = "90%"),
    # Add your sidebar content here
    ui_card(
      card_title("FILTERS"),

      card_body(
        #style = "overflow: hidden;",
        selectInput(inputId = "geoid", label = "AREA", choices = read_rds("data/calculator/appdata.rds"), selected = 36109, width = "200px"),
        selectInput(inputId = "pollutant", label = "POLLUTANT",
                    choices = set_names(x = read_rds("data/core.rds")$choices$pollutant %>% as.numeric(),
                                        nm = read_rds("data/core.rds")$choices$pollutant %>% names()),
                    selected = 98, width = "200px"),
        selectInput(inputId = "by", label = "AGGREGATION",
                    choices = c("Overall" = 16,
                                "with Source Mix" = 17,
                                "with Fuel Mix" = 19,
                                "with Source & Fuel Mix" = 21,
                                "with Regulatory Class Mix" = 18,
                                "with Road Mix" = 20,
                                "by Source" = 8,
                                "by Fuel Type" = 14,
                                "by Regulatory Class" = 12,
                                "by Road Type" = 15),
                    selected = 16, width = "200px"),
        selectInput(inputId = "category", label = "SUBTYPE",
                    choices = c("Overall" = 0),
                    selected = 0, width = "200px"),
        selectInput(inputId = "modeltype", label = "VARIABLES",
                    choices = c("Few" = "a",
                                "Several" = "b",
                                "Many" = "c"),
                    selected = "a", width = "100px"),
        selectInput(inputId = "modelrank", label = "RANK",
                    choices = c("1" = "1", "2" = "2", "3" = "3"),
                    selected = 1, width = "100px")
      )
    ),

    ui_card(
      card_title("OPTIONS"),
      card_body(
        style = "overflow: hidden;",
        selectInput(inputId = "startyear", label = "START YEAR", choices = 1990:2060,
                    selected = stringr::str_sub(Sys.Date(), 1,4), width = "200px"),
        selectInput(inputId = "unit", label = "UNIT", choices = c("tons" = "t"), selected = "t", width = "200px"),
        selectInput(inputId = "scaling", label = "SCALE INPUTS",
                    choices = c("per 1" = 1, "per thousand" = 1e3, "per million" = 1e6, "per billion" = 1e9),
                    selected = 1e3, width = "200px"),
        selectInput(inputId = "rounding", label = "ROUND INPUTS",
                    choices = c("Nearest Digit" = "TRUE", "No Rounding" = "FALSE"), selected = "TRUE", width = "200px")
      )
    )

  )

  ### ACCURACY ####################################
  ui_accuracy = ui_card(
    style = "height: 150px; max-height: 100%;",
    card_header(
      style = "background-color: #2fa4e7;",
      tags$b("Accuracy", style = "color: #FFFFFF;"),
      tooltip(
        .title = paste0(
          "<b>Measure: Adjusted R<sup>2</sup></b>",
          "<br>",
          "<i>Definition</i>: % of variation in emissions explained by the model. Adjusted for # of predictors. Higher is better.",
          "<br>",
          "<i>Range</i>: 0 - 100%."),
        icon("info-circle")  )  ),
    card_body_fill(
      #textOutput(outputId = "accuracy", container = tags$h5),
      gaugeOutput(outputId = "accuracygauge"),
      style = css(`max-height` = "100%", padding = "0px", `margin-right` = "0px", `margin-bottom` = "0px", `margin-top` = "0px", `margin-left` = "0px"))
  )

  ### AVERAGE ERROR ####################################
  ui_sigma = ui_card(
    style = "height: 150px; max-height: 100%;",

    card_header(
      style = "background-color: #2fa4e7;",
      tags$b("Average Error", style = "color: #FFFFFF"),
      tooltip(
        .title = paste0(
          "<b>Measure: Sigma (RMSE)</b>",
          "<br>",
          "<i>Definition</i>: Average error in model predictions, in units of emissions. Lower is better. Even if large, if error is just a small percentage of the range, then it is small relative to overall range.",
          "<br>",
          "<i>Range</i>: 0 - Infinity."),
        icon("info-circle") ) ),
    card_body_fill(
      textOutput(outputId = "sigma", container = tags$h3),
      textOutput(outputId = "percentrange"),
      style = css(`max-height` = "100%", padding = "0px",
                  `text-align` = 'center', `vertical-align` = "middle",
                  `margin-right` = "0px", `margin-bottom` = "0px",
                  `margin-top` = "0px", `margin-left` = "0px"))
  )

  ### RANGE ####################################
  ui_range = ui_card(
    style = "height: 150px; max-height: 100%;",

    card_header(
      style = "background-color: #2fa4e7;",
      tags$b("Range", style = "color: #FFFFFF"),
      tooltip(
        .title = paste0(
          "<b>Measure: Range</b>",
          "<br>",
          "<i>Definition</i>: Distance from Lowest to Highest level of Emissions seen in this area, according to our baseline model data."),
        icon("info-circle")  ) ),
    card_body_fill(
      textOutput(outputId = "yrange", container = tags$h3),
      textOutput(outputId = "yinterval"),
      style = css(`max-height` = "100%", padding = "0px",
                  `text-align` = 'center', `vertical-align` = "middle",
                  `margin-right` = "0px", `margin-bottom` = "0px",
                  `margin-top` = "0px", `margin-left` = "0px"))
  )

  ### SAMPLE ####################################
  ui_sample = ui_card(
    style = "height: 150px; max-height: 100%;",

    card_header(
      style = "background-color: #2fa4e7;",
      tags$b("Sample", style = "color: #FFFFFF;"),
      tooltip(
        .title = paste0(
          "<b>Measure: Sample Size (N)</b>",
          "<br>",
          "<i>Definition</i>: Total number of yearly observations and range of years of MOVES data analyzed in model."),
        icon("info-circle")  ) ),
    card_body_fill(
      textOutput(outputId = "nyears", container = tags$h3),
      textOutput(outputId = "yearrange"),
      style = css(`max-height` = "100%", padding = "0px",
                  `text-align` = 'center', `vertical-align` = "middle",
                  `margin-right` = "0px", `margin-bottom` = "0px",
                  `margin-top` = "0px", `margin-left` = "0px"))
  )

  ## ABOUT ###################################
  ui_about = card_body_fill(
    tags$ul(
      tags$li("This equation shows the most accurate model of emissions over time in <b><i>your local area</b></i>, according to our algorithms." %>% HTML()),
      tags$li("Our models are trained on estimates from the EPA's MOVES software, the gold standard in the US for emissions modeling."),
      tags$li("MOVES analyses are highly precise, but computationally expensive. Our MOVESLite system delivers immediate estimates, with an acceptable margin of error, to aid scenario-building and decision-making."),
      tags$li(
        tags$b("Model Equation"),
        tooltip(
          .title = paste0(
            "<b>Measure: Model Equation</b>",
            "<br>",
            "<i>Definition</i>: Best-fitting equation for estimating emissions based on local activity levels. Add your local activity levels below, and this model will predict the expected level of emissions."),
          icon("info-circle") ),
        uiOutput(outputId = "equation")),
      tags$li(textOutput(outputId = "note_scaling", inline = TRUE) )
    )
  )
  # Multi Columns
  ui_more = column_wrap(
    width = .25, gap = "2px",
    style = css(
      `padding-left` = "0px",
      `padding-right` = "0px",
      grid_template_columns = "1fr 1fr"),
    l = list(
      card_body_fill(
        tags$b("Subscribe for More Features!"),
        textInput(inputId = "email", label = "EMAIL", width = "200px",
                  placeholder = "youremail@gmail.com")
      ),
      card_body_fill(
        actionButton(inputId = "submit_email", label = "SUBMIT",
                     icon = icon("person"), width = "100px")    )
    )
  )

  # TOOLBAR ###########################################
  ui_toolbar = navs_tab_card(
    id = "toolbar",
    title = tags$b("MODEL"), selected = "metrics",
    ## NAV: ABOUT ###################################
    nav(value = "metrics", title = "METRICS", icon = icon("weight-scale"),
        column_wrap(
          width = .25, gap = "2px",
          style = css(`padding-left` = "0px", `padding-right` = "0px", grid_template_columns = "1fr 1fr 1fr 1fr"),
          l = list( ui_accuracy, ui_sigma, ui_range, ui_sample)   )   ),
    ## NAV: ABOUT ###################################
    nav(value = "model", title = "ABOUT MODEL", icon = icon("magnifying-glass-chart"),  ui_about),
    ## NAV: DOWNLOAD ###############################
    bslib::nav_item(
      downloadButton(label = "DOWNLOAD",
                     outputId = "download", icon = icon("download"),
                     style = "background-image: none;")),
    ## NAV: MORE ####################################
    nav(title = "MORE", icon = icon("gear"), value = "more", ui_more) # end of nav

  )

  # GRAPHICS ###################################################
  ui_graphics = column_wrap(
    width = 0.5, fill = TRUE,
    style = css(grid_template_columns = '1fr 3fr'),
    l = list(
      ui_card(
        card_header(
          style = "background-color: #2fa4e7;",
          tags$b("Average Change in Emissions", style = "color: #FFFFFF;", icon("leaf"))),
        card_body_fill(
          textOutput(outputId = "stat", container = tags$h3),
          " per year",
          tags$br(),
          textInput(inputId = "scenario_benchmark", label = "BASELINE", value = "Baseline", width = "100%"),
          textInput(inputId = "scenario_yours", label = "YOUR SCENARIO", value = "Your Scenario", width = "100%"),

          style = "text-align: center; vertical-align: middle; max-height: 100%;"
        ),
        style = "max-height: 100%;"),

      # PLOT ###################################################
      card(plotlyOutput("visual", height = "300px"), style = "max-height: 100%; min-height: 300px;")
    )
  ) %>%
    fluidRow()

  # SCENARIOS ##############################################
  ui_scenarios = fluidRow(
    # BUTTONS ################################################
    card(
      style = css(
        `border-color` = 'transparent',
        `padding-left` = '0px',
        `padding-right` = '20px',
        `padding-bottom` = '2px',
        `padding-top` = '0px',
        `margin-top` = '10px',
        `margin-left` = '10px',
        `margin-right` = '0px',
        `margin-bottom` = '2px'),
      card_header(
        tags$b("INPUTS"),
        style = css(`background-color` = "#2fa4e7",
                    `color` = "#FFFFFF")),
      card_body(
        style = css(`padding-left` = "15px",
                    `padding-right` = "0px",
                    `padding-top` = "5px",
                    `padding-bottom` = "5px",
                    `margin` = "0px",
                    `overflow` = "hidden"),
        div(id = "inputsets"),
        actionButton(inputId = "add_set", icon = icon("circle-plus"), label =  "Add Input Set"),
        actionButton(inputId = "remove_set", icon = icon("circle-minus"), label =  "Remove Input Set"),
        actionButton(inputId = "reset", label = "Reset Inputs", icon = icon("arrows-rotate"))
      )
    )
  )

  #add this file and collapsible nature should work.

  ui <- fluidPage(
    # Add head and styling
    tags$head( tags$style( mycss ),  tags$script(HTML(js))  ),
    # Add theme
    theme = bslib::bs_theme(version = 5, bootswatch = "cerulean"),
    shinyjs::useShinyjs(),
    # Add title
    ui_title,

    # Layout Sidebar vs. Main Panel
    sidebarLayout(
      position = "left", fluid = TRUE,
      sidebarPanel = ui_sidebar,
      # In the main panel...
      mainPanel(
        width = 9,
        style = css(
          `max-width` = "100%",
          `min-width` = "300px",
          `margin-left` = "0px",
          `margin-right` = "0px",
          `margin-top` = "0px",
          `margin-bottom` = "0px",
          `padding-left` = "0px",
          `padding-top` = "5px"),
        # TOOLBAR ###################################
        ui_toolbar,   # end of navtab
        # OUTPUT GRAPHICS ################################################
        ui_graphics,
        # SCENARIO INPUT SETS ################################################
        ui_scenarios,

        # DIAGNOSTICS ###########################################################
        ui_card( shiny::verbatimTextOutput(outputId = "diagnostics", placeholder = TRUE) )


      )
    )
  )

  return(ui)
}




#' @name server()
#' @author Tim Fraser
#' @description User Interface function for Calculator ShinyApp

server = function(input, output, session) {

  # 1. SETUP #################################
  ## 1.1. Functions #######################
  load("data/calculator/ui_card.rda")
  load("data/calculator/column_wrap.rda")
  load("data/calculator/ui_set.rda")

  load("data/moveslite/connect.rda")
  load("data/moveslite/query.rda")
  load("data/moveslite/query_download.rda")
  load("data/moveslite/query_by.rda")
  load("data/moveslite/query_aggregate.rda")
  load("data/moveslite/find_transformation.rda")
  load("data/moveslite/convert.rda")
  load("data/moveslite/setx.rda")
  load("data/moveslite/estimate.rda")
  load("data/moveslite/project.rda")
  load("data/moveslite/find_stats.rda")
  load("data/moveslite/print_note.rda")
  load("data/moveslite/print_equation.rda")

  load("data/moveslite/transformations.rda")
  load("data/moveslite/keywords.rda")
  load("data/moveslite/formulas.rda")

  realyears = seq(from = 1990, to = 2060, by = 5) %>% .[.!=1995]

  # Load this background data
  db = connect("cov")
  cov = db %>% tbl("areas") %>% filter(level %in% c("county", "state", "nation")) %>% collect()
  dbDisconnect(db); remove(db)

  # Initialize reactiveValues object to store input sets
  sets <- reactiveValues()
  sets$count <- 0

  ## 1.2. Updates ##############################

  ### 1.2.1 Year =======================================
  # Get the range of years from selected start year (PRESENT) to the end of time
  observe({ sets$years = input$startyear:2060 }) %>% bindEvent({input$startyear})


  ### 1.2.3 Pollutant =======================================
  observe({
    req(input$geoid)
    # input = list(geoid = "36109")
    .geoid = input$geoid
    db = connect("data")
    .table = paste0("d", .geoid)
    valid_pollutants = db %>% tbl(.table) %>% select(pollutant) %>% distinct() %>% collect() %>% with(pollutant)
    dbDisconnect(db)

    # Get my pollutants
    mypollutants = read_rds("data/core.rds")$choices$pollutant
    # Example:
    # valid_pollutants = function(){ c(98, 2) }
    # Narrow into just valid pollutants for that geoid
    mypollutants = mypollutants[ mypollutants %in% valid_pollutants]
    # Make named vector
    mypollutants = purrr::set_names(x = mypollutants %>% as.numeric(), nm = mypollutants %>% names())

    # Update the inputs
    updateSelectInput(inputId = "pollutant", choices = mypollutants, selected = 98)

  }) %>% bindEvent({input$geoid})

  ### 1.2.2 Category =======================================
  # Whenever you change the 'by' Aggregation field, the options for Category should change too.
  categorychoices = reactive({
    req(input$by)
    # Update choices...
    bytypes = tribble(
      ~id, ~name,
      16, "overall",
      17, "overall",
      18, "overall",
      19, "overall",
      20, "overall",
      21, "overall",
      8, "sourcetype",
      12, "regclass",
      14, "fueltype",
      15, "roadtype"
    )
    byname = bytypes %>% filter(id == input$by) %>% with(name)
    result = keywords %>% filter(type == byname) %>% with(set_names(as.integer(.$id), .$term))
    return(result)
  })

  observe({
    updateSelectInput(
      inputId = "category", label = "SUBTYPE",
      choices = categorychoices(), selected = categorychoices()[1])
  }) %>% bindEvent({ categorychoices() })

  # 2. QUERIES #####################################

  ## 2.1 DOWNLOAD FROM DB ###################################

  download = reactive({
    # Requires all before proceeding
    req(input$geoid, input$pollutant)
    # Connect to data database
    db = connect("data")
    # Query the table for that polllutant
    result = query_download(
      .db = db, .table = paste0("d", input$geoid), .pollutant = input$pollutant,
      .vars = c("by", "sourcetype", "regclass", "fueltype", "roadtype", "year", "emissions", "vmt", "vehicles", "starts", "sourcehours"))
    # Disconnect from database
    dbDisconnect(db)
    # Return the data!
    print("---download()"); return(result)
  }) %>% bindCache(input$geoid, input$pollutant)

  download_query_by = reactive({
    # Select just those variables from download()
    req(download())
    req(input$category, input$by)
    .by = as.integer(input$by); .category = as.integer(input$category)
    result = query_by(data = download(), .by = .by, .category = .category)
    # Get variables
    nums = result %>%
      # Grab just the numeric variables
      select_if(.predicate = function(x){is.numeric(x)}) %>%
      # Remove any of these variables, if they got flagged
      select(-any_of(c("year", "geoid", "pollutant", "sourcetype", "roadtype", "fueltype", "regclass"))) %>%
      # Return the names.
      names()
    # Grab just variables; don't need geoid, pollutant, or by or categories anymore
    result = result %>% select(emissions, year, any_of(nums))
    # Return and print completion message
    print("---download_query_by()"); return(result)
  }) %>% bindCache(input$geoid, input$pollutant, input$category, input$by)


  download_setx = reactive({
    req(download_query_by())
    data = download_query_by()
    check_n = nrow(data)
    if(!is.null(check_n)){
      if(check_n > 0){
        # Interpolate out unknown values for me each year-by-year.
        result = setx(
          data = data,
          .newx = tibble(year = seq(from = 1990, to = 2060, by = 1) ),
          .cats = "year", .context = FALSE) %>%
          # Drop the custom field - we don't need it yet
          filter(type == "benchmark") %>% select(-type)
        # Return and print completion message
        print("---download_setx()"); return(result)
      }
    }
  }) %>% bindCache(input$geoid, input$pollutant, input$category, input$by)


  ## 2.4 DEFAULT YEARLY DATA (FORMATTED) #######################################
  default = reactive({
    req(download_setx())
    # Get the names in default() [which already have dropped irrelevant columns]
    nums = download_setx() %>% names()
    nums = nums[!nums %in% c("emissions", "year")]
    # Rescale them
    result = download_setx() %>% mutate(across(.cols = any_of(nums), .fns = ~.x / as.numeric(input$scaling)))
    # Round them!
    if(input$rounding == "TRUE"){ result = result %>% mutate(across(.cols = any_of(nums), .fns = ~round(.x, digits = 0))) }
    # Return the output
    print("---default()"); return(result)
  }) %>%
    bindCache(input$geoid, input$pollutant, input$by, input$category, input$scaling, input$rounding)



  # 3. MODELING ###############################

  ## 3.1 MODEL #################################
  # Estimate the model based on default data (NOT default interpolated yearly data)
  model = reactive({
    # Invalidate if these change
    req(default(), input$modeltype, input$modelrank)
    # Get the formula dataframe
    yourformula = formulas %>%
      filter(modeltype == input$modeltype, rank == as.integer(input$modelrank))
    # Estimate this model (submitting a .formula overrides any other settings)
    model = estimate(
      # Filter to just real years
      data = default() %>% filter(year %in% realyears),
      .formula = yourformula$formula[[1]])
    # Assign the contents of yourformula() to the model object
    model$yourformula <- yourformula
    # Return and Print Message
    print("---model()"); return(model)
  }) %>% bindCache(input$geoid, input$pollutant, input$by, input$category, input$scaling, input$rounding, input$modeltype, input$modelrank)



  ## 3.2 GOF STATS ###########################################################
  stats = reactive({
    req(model(), input$unit)
    find_stats(m = model(), .unit = input$unit)
  }) %>% bindCache(input$geoid, input$pollutant, input$by, input$category, input$scaling, input$rounding, input$modeltype, input$modelrank, input$unit)


  # 4. SCENARIO MODULES ####################################

  ## 4.1 ADD INPUT SET #####################################
  observe({
    # Require both before proceeding
    req(default(), input$scaling, input$add_set)

    # Update the counter
    sets$count <- sets$count + 1
    setcount <- sets$count

    data = reactive({ default() %>% filter(year %in% sets$years) })
    vars = reactive({ model()$yourformula$vars %>% unlist() })
    insertUI(
      selector = "#inputsets",
      where = "beforeBegin",
      ui = ui_set(
        setcount = setcount,
        data = data(),
        vars = vars(),
        scaling = input$scaling) )

  }) %>% bindEvent({input$add_set })

  ## 4.2 REMOVE INPUT SETS ################################
  observe({
    # Get the number of input sets
    setcount <- sets$count
    # If there is at least 1 input set active...
    if (setcount > 0) {
      # Get the namespace
      ns <- NS(paste0("set", "_", setcount))
      # Remove the input set
      removeUI(selector = paste0("#", ns("inputset")), multiple = TRUE)
      # Subtract 1 from the number of input sets
      sets$count <- sets$count - 1
    }
  }) %>% bindEvent({input$remove_set})

  ## 4.3 RESET #############################################
  observe({

    setcount <- sets$count

    # If there are any existing inputsets,
    if(setcount > 0){
      # Iteratively remove them
      lapply(X = 1:setcount, FUN = function(i){
        ns = NS(paste0("set", "_", i))
        removeUI(selector = paste0("#", ns("inputset")), multiple = TRUE)
      })
      # Reduce set counter to zero
      sets$count <- 0
    }

  }) %>% bindEvent({input$reset; model()}, ignoreInit = TRUE)


  observe({
    setcount = sets$count
    sets$zero = if_else(setcount == 0, TRUE, FALSE)
    #print(paste0("---zero-sets: ", sets$zero))
  }) %>% bindEvent({sets$count})

  # When there are zero sets, add a starter set.
  observe({
    if(sets$zero == TRUE){
      # Then add 1 set
      sets$count <- sets$count + 1
      setcount <- sets$count

      # Add one set
      data = default() %>% filter(year %in% sets$years)
      vars = model()$yourformula$vars %>% unlist()
      insertUI(
        selector = "#inputsets",
        where = "beforeBegin",
        ui = ui_set(
          setcount = setcount,
          data = data,
          vars = vars,
          scaling = input$scaling) )
    }
  }) %>% bindEvent({sets$zero})



  ## 4.3 UPDATE PREDICTIONS ########################################
  ydata = reactive({
    # Only proceed after default_yearly() has been calculated
    req( default(), model() )
    # Get the number of input sets
    setcount = sets$count
    # For these variables of interest
    vars = model()$yourformula$vars %>% unlist()
    vars = vars[!vars %in% c("emissions")]

    # Given at least 1 set of inputs...
    if(setcount > 0){
      # Don't continue until at least the first has shown up
      ns <- NS(paste0("set", "_", 1))
      req(input[[ ns("year") ]])

      # For each set, get xvar data
      xdata = 1:setcount %>%
        map_dfr(.f = ~{
          ns <- NS(paste0("set", "_", .x)); # # Get namespace of input set
          # For as many variables as desired...
          vars %>% # Extract columns of these variables
            map(.f = ~{ tibble( input[[ ns(.x) ]] %>% as.numeric() ) %>% set_names(., nm = .x) }) %>% # Bundle each into a tibble with proper names
            keep(~nrow(.) > 0) %>% # Keep just valid, non-empty data.frames
            bind_cols()  } ) # And bind the columns together!

      # Generate statistics
      ydata = project(m = model(), data = default(), .newx = xdata,
                      .cats = "year", .exclude = "geoid", .context = TRUE, .realyears = realyears)
      # Calculate change in emissions from default
      ydata = ydata %>%
        left_join(
          by = "year",
          y = default() %>% select(year, default = emissions)) %>%
        mutate(change = emissions - default)
      # Whenever xdata() changes, update the predictions
      print("---ydata()"); return(ydata)
    }
  })


  ## 4.4. RENDER OUTPUT VALUES ###################################################################
  observe({
    setcount = sets$count
    if(setcount > 0){ # eg. setcount = 3
      # Get the names of any input variables present; drop emissions
      vars = model()$yourformula$vars %>% unlist(); vars = vars[!vars %in% c("emissions")]
      # Get exact slice of n (setcount) rows of data
      customdata = reactive({ ydata() %>% filter(type == "custom") %>% arrange(year) %>% select(emissions, change) })
      # Let's generate a reactive value, containing all the inputs from that set.
      lapply(X = 1:nrow(customdata()), FUN = function(i){
        # Get reactive vector of names
        input_names = reactive({ names(input)[ stringr::str_detect(names(input), pattern = paste0("set_", i)) ] })
        # Assign the output
        output[[ paste0("set_", i, "-emissions") ]] <- renderText({ customdata()$emissions[i] }) %>% bindEvent(input, { input_names() })
        output[[ paste0("set_", i, "-change") ]] <- renderText({ customdata()$change[i] }) %>% bindEvent(input, { input_names() })
      })
    }
  }) %>% bindEvent({ydata()})


  ## 3.3 STATISTICS ####################################################



  # DIAGNOSTICS ###############################


  # 5. OUTPUT #################################

  ## 5.1 QIs ##################################
  qi = reactive({
    req(ydata(), input$scenario_benchmark, input$scenario_yours, input$geoid, input$startyear, input$unit, input$pollutant)
    load("data/calculator/qi_scenario.rda")
    qi_scenario(input = input, ydata = ydata())
  })

  ## 5.2 DOWNLOAD #############################################
  output$download <- downloadHandler(
    contentType = "text/csv", filename = function(){"data.csv" },
    content = function(file){  qi()$data %>% select(-text) %>% write_csv(file) })


  ## 5.3 VISUAL ###############################
  output$visual = renderPlotly({
    req(qi())
    load("data/calculator/h_scenario.rda")
    h_scenario(qi())
  })

  ## 5.4 Equation =======================================
  output$equation = renderUI({ req(model()); print_equation(model())  })

  ### 5.5 Note Scaling ============================

  ## 5.6 GOF Stats =====================================
  observe({
    req(stats())  # When stats() updates, update all of these at once
    # Generate a key
    modelid = paste(input$geoid, input$pollutant, input$by, input$category, input$scaling, input$rounding, input$modeltype, input$modelrank, input$unit, sep = "-")
    if(nrow(stats()) > 0){
      output$sigma = renderText({ stats()$sigma }) %>% bindCache(modelid)
      output$percentrange = renderText({ stats()$percentrange }) %>% bindCache(modelid)
      output$yearrange = renderText({ stats()$yearrange }) %>% bindCache(modelid)
      output$nyears = renderText({  stats()$nyears }) %>% bindCache(modelid)
      output$yrange = renderText({  stats()$yrange }) %>% bindCache(modelid)
      output$yinterval = renderText({ stats()$yinterval }) %>% bindCache(modelid)
      output$accuracygauge = renderGauge({
        gauge(value = stats()$accuracygauge, min = 0, max = 100, symbol = "%", abbreviateDecimals = 1,
              sectors = gaugeSectors(success = c(95, 100), warning = c(90, 0.95), danger = c(0, 90)))
      }) %>% bindCache(modelid)
      # Add a note to the model (unrelated to GOF stats, but helpful to put it here.)
      output$note_scaling = renderText({
        print_note(yourformula = model()$yourformula, scaling = input$scaling, unit = input$unit)
      }) %>% bindCache(modelid)
    }
  }) %>% bindEvent({stats()})

  ## 5.7 Avg. Treatment Effect ============================
  output$stat = renderText({
    req(ydata(), input$unit)
    setcount = sets$count;
    if(setcount > 0){ stat = ydata() %>% filter(type == "custom") %>% with(change) %>% mean(na.rm = TRUE) %>%
      number(accuracy = 0.1, style_positive = "plus", style_negative = "minus", scale_cut = cut_si(unit = input$unit))
    }else{ stat = "..." }
    return(stat)
  }) %>% bindEvent({ydata(); input$unit})


  # Z. Diagnostics #####################################
  output$diagnostics <- renderPrint({ default() %>% glimpse()  })



}

#' @name server()
#' @author Tim Fraser
#' @description User Interface function for Calculator ShinyApp

server = function(input, output, session) {

  # 1. SETUP #################################
  ## 1.1. Functions #######################
  load("data/calculator/ui_card.rda")
  load("data/calculator/column_wrap.rda")
  load("data/calculator/ui_set.rda")

  load("data/moveslite/connect.rda")
  load("data/moveslite/query.rda")
  load("data/moveslite/query_download.rda")
  load("data/moveslite/query_by.rda")
  load("data/moveslite/query_aggregate.rda")
  load("data/moveslite/find_transformation.rda")
  load("data/moveslite/convert.rda")
  load("data/moveslite/setx.rda")
  load("data/moveslite/estimate.rda")
  load("data/moveslite/project.rda")
  load("data/moveslite/find_stats.rda")
  load("data/moveslite/print_note.rda")
  load("data/moveslite/print_equation.rda")

  load("data/moveslite/transformations.rda")
  load("data/moveslite/keywords.rda")
  load("data/moveslite/formulas.rda")

  realyears = seq(from = 1990, to = 2060, by = 5) %>% .[.!=1995]

  # Load this background data
  db = connect("cov")
  cov = db %>% tbl("areas") %>% filter(level %in% c("county", "state", "nation")) %>% collect()
  dbDisconnect(db); remove(db)

  # Initialize reactiveValues object to store input sets
  sets <- reactiveValues()
  sets$count <- 0

  ## 1.2. Updates ##############################

  ### 1.2.1 Year =======================================
  # Get the range of years from selected start year (PRESENT) to the end of time
  observe({ sets$years = input$startyear:2060 }) %>% bindEvent({input$startyear})


  ### 1.2.3 Pollutant =======================================
  observe({
    req(input$geoid)
    # input = list(geoid = "36109")
    .geoid = input$geoid
    db = connect("data")
    .table = paste0("d", .geoid)
    valid_pollutants = db %>% tbl(.table) %>% select(pollutant) %>% distinct() %>% collect() %>% with(pollutant)
    dbDisconnect(db)

    # Get my pollutants
    mypollutants = read_rds("data/core.rds")$choices$pollutant
    # Example:
    # valid_pollutants = function(){ c(98, 2) }
    # Narrow into just valid pollutants for that geoid
    mypollutants = mypollutants[ mypollutants %in% valid_pollutants]
    # Make named vector
    mypollutants = purrr::set_names(x = mypollutants %>% as.numeric(), nm = mypollutants %>% names())

    # Update the inputs
    updateSelectInput(inputId = "pollutant", choices = mypollutants, selected = 98)

  }) %>% bindEvent({input$geoid})

  ### 1.2.2 Category =======================================
  # Whenever you change the 'by' Aggregation field, the options for Category should change too.
  categorychoices = reactive({
    req(input$by)
    # Update choices...
    bytypes = tribble(
      ~id, ~name,
      16, "overall",
      17, "overall",
      18, "overall",
      19, "overall",
      20, "overall",
      21, "overall",
      8, "sourcetype",
      12, "regclass",
      14, "fueltype",
      15, "roadtype"
    )
    byname = bytypes %>% filter(id == input$by) %>% with(name)
    result = keywords %>% filter(type == byname) %>% with(set_names(as.integer(.$id), .$term))
    return(result)
  })

  observe({
    updateSelectInput(
      inputId = "category", label = "SUBTYPE",
      choices = categorychoices(), selected = categorychoices()[1])
  }) %>% bindEvent({ categorychoices() })

  # 2. QUERIES #####################################

  ## 2.1 DOWNLOAD FROM DB ###################################

  download = reactive({
    # Requires all before proceeding
    req(input$geoid, input$pollutant)
    # Connect to data database
    db = connect("data")
    # Query the table for that polllutant
    result = query_download(
      .db = db, .table = paste0("d", input$geoid), .pollutant = input$pollutant,
      .vars = c("by", "sourcetype", "regclass", "fueltype", "roadtype", "year", "emissions", "vmt", "vehicles", "starts", "sourcehours"))
    # Disconnect from database
    dbDisconnect(db)
    # Return the data!
    print("---download()"); return(result)
  }) %>% bindCache(input$geoid, input$pollutant)

  download_query_by = reactive({
    # Select just those variables from download()
    req(download())
    req(input$category, input$by)
    .by = as.integer(input$by); .category = as.integer(input$category)
    result = query_by(data = download(), .by = .by, .category = .category)
    # Get variables
    nums = result %>%
      # Grab just the numeric variables
      select_if(.predicate = function(x){is.numeric(x)}) %>%
      # Remove any of these variables, if they got flagged
      select(-any_of(c("year", "geoid", "pollutant", "sourcetype", "roadtype", "fueltype", "regclass"))) %>%
      # Return the names.
      names()
    # Grab just variables; don't need geoid, pollutant, or by or categories anymore
    result = result %>% select(emissions, year, any_of(nums))
    # Return and print completion message
    print("---download_query_by()"); return(result)
  }) %>% bindCache(input$geoid, input$pollutant, input$category, input$by)


  download_setx = reactive({
    req(download_query_by())
    data = download_query_by()
    check_n = nrow(data)
    if(!is.null(check_n)){
      if(check_n > 0){
        # Interpolate out unknown values for me each year-by-year.
        result = setx(
          data = data,
          .newx = tibble(year = seq(from = 1990, to = 2060, by = 1) ),
          .cats = "year", .context = FALSE) %>%
          # Drop the custom field - we don't need it yet
          filter(type == "benchmark") %>% select(-type)
        # Return and print completion message
        print("---download_setx()"); return(result)
      }
    }
  }) %>% bindCache(input$geoid, input$pollutant, input$category, input$by)


  ## 2.4 DEFAULT YEARLY DATA (FORMATTED) #######################################
  default = reactive({
    req(download_setx())
    # Get the names in default() [which already have dropped irrelevant columns]
    nums = download_setx() %>% names()
    nums = nums[!nums %in% c("emissions", "year")]
    # Rescale them
    result = download_setx() %>% mutate(across(.cols = any_of(nums), .fns = ~.x / as.numeric(input$scaling)))
    # Round them!
    if(input$rounding == "TRUE"){ result = result %>% mutate(across(.cols = any_of(nums), .fns = ~round(.x, digits = 0))) }
    # Return the output
    print("---default()"); return(result)
  }) %>%
    bindCache(input$geoid, input$pollutant, input$by, input$category, input$scaling, input$rounding)



  # 3. MODELING ###############################

  ## 3.1 MODEL #################################
  # Estimate the model based on default data (NOT default interpolated yearly data)
  model = reactive({
    # Invalidate if these change
    req(default(), input$modeltype, input$modelrank)
    # Get the formula dataframe
    yourformula = formulas %>%
      filter(modeltype == input$modeltype, rank == as.integer(input$modelrank))
    # Estimate this model (submitting a .formula overrides any other settings)
    model = estimate(
      # Filter to just real years
      data = default() %>% filter(year %in% realyears),
      .formula = yourformula$formula[[1]])
    # Assign the contents of yourformula() to the model object
    model$yourformula <- yourformula
    # Return and Print Message
    print("---model()"); return(model)
  }) %>% bindCache(input$geoid, input$pollutant, input$by, input$category, input$scaling, input$rounding, input$modeltype, input$modelrank)



  ## 3.2 GOF STATS ###########################################################
  stats = reactive({
    req(model(), input$unit)
    find_stats(m = model(), .unit = input$unit)
  }) %>% bindCache(input$geoid, input$pollutant, input$by, input$category, input$scaling, input$rounding, input$modeltype, input$modelrank, input$unit)


  # 4. SCENARIO MODULES ####################################

  ## 4.1 ADD INPUT SET #####################################
  observe({
    # Require both before proceeding
    req(default(), input$scaling, input$add_set)

    # Update the counter
    sets$count <- sets$count + 1
    setcount <- sets$count

    data = reactive({ default() %>% filter(year %in% sets$years) })
    vars = reactive({ model()$yourformula$vars %>% unlist() })
    insertUI(
      selector = "#inputsets",
      where = "beforeBegin",
      ui = ui_set(
        setcount = setcount,
        data = data(),
        vars = vars(),
        scaling = input$scaling) )

  }) %>% bindEvent({input$add_set })

  ## 4.2 REMOVE INPUT SETS ################################
  observe({
    # Get the number of input sets
    setcount <- sets$count
    # If there is at least 1 input set active...
    if (setcount > 0) {
      # Get the namespace
      ns <- NS(paste0("set", "_", setcount))
      # Remove the input set
      removeUI(selector = paste0("#", ns("inputset")), multiple = TRUE)
      # Subtract 1 from the number of input sets
      sets$count <- sets$count - 1
    }
  }) %>% bindEvent({input$remove_set})

  ## 4.3 RESET #############################################
  observe({

    setcount <- sets$count

    # If there are any existing inputsets,
    if(setcount > 0){
      # Iteratively remove them
      lapply(X = 1:setcount, FUN = function(i){
        ns = NS(paste0("set", "_", i))
        removeUI(selector = paste0("#", ns("inputset")), multiple = TRUE)
      })
      # Reduce set counter to zero
      sets$count <- 0
    }

  }) %>% bindEvent({input$reset; model()}, ignoreInit = TRUE)


  observe({
    setcount = sets$count
    sets$zero = if_else(setcount == 0, TRUE, FALSE)
    #print(paste0("---zero-sets: ", sets$zero))
  }) %>% bindEvent({sets$count})

  # When there are zero sets, add a starter set.
  observe({
    if(sets$zero == TRUE){
      # Then add 1 set
      sets$count <- sets$count + 1
      setcount <- sets$count

      # Add one set
      data = default() %>% filter(year %in% sets$years)
      vars = model()$yourformula$vars %>% unlist()
      insertUI(
        selector = "#inputsets",
        where = "beforeBegin",
        ui = ui_set(
          setcount = setcount,
          data = data,
          vars = vars,
          scaling = input$scaling) )
    }
  }) %>% bindEvent({sets$zero})



  ## 4.3 UPDATE PREDICTIONS ########################################
  ydata = reactive({
    # Only proceed after default_yearly() has been calculated
    req( default(), model() )
    # Get the number of input sets
    setcount = sets$count
    # For these variables of interest
    vars = model()$yourformula$vars %>% unlist()
    vars = vars[!vars %in% c("emissions")]

    # Given at least 1 set of inputs...
    if(setcount > 0){
      # Don't continue until at least the first has shown up
      ns <- NS(paste0("set", "_", 1))
      req(input[[ ns("year") ]])

      # For each set, get xvar data
      xdata = 1:setcount %>%
        map_dfr(.f = ~{
          ns <- NS(paste0("set", "_", .x)); # # Get namespace of input set
          # For as many variables as desired...
          vars %>% # Extract columns of these variables
            map(.f = ~{ tibble( input[[ ns(.x) ]] %>% as.numeric() ) %>% set_names(., nm = .x) }) %>% # Bundle each into a tibble with proper names
            keep(~nrow(.) > 0) %>% # Keep just valid, non-empty data.frames
            bind_cols()  } ) # And bind the columns together!

      # Generate statistics
      ydata = project(m = model(), data = default(), .newx = xdata,
                      .cats = "year", .exclude = "geoid", .context = TRUE, .realyears = realyears)
      # Calculate change in emissions from default
      ydata = ydata %>%
        left_join(
          by = "year",
          y = default() %>% select(year, default = emissions)) %>%
        mutate(change = emissions - default)
      # Whenever xdata() changes, update the predictions
      print("---ydata()"); return(ydata)
    }
  })


  ## 4.4. RENDER OUTPUT VALUES ###################################################################
  observe({
    setcount = sets$count
    if(setcount > 0){ # eg. setcount = 3
      # Get the names of any input variables present; drop emissions
      vars = model()$yourformula$vars %>% unlist(); vars = vars[!vars %in% c("emissions")]
      # Get exact slice of n (setcount) rows of data
      customdata = reactive({ ydata() %>% filter(type == "custom") %>% arrange(year) %>% select(emissions, change) })
      # Let's generate a reactive value, containing all the inputs from that set.
      lapply(X = 1:nrow(customdata()), FUN = function(i){
        # Get reactive vector of names
        input_names = reactive({ names(input)[ stringr::str_detect(names(input), pattern = paste0("set_", i)) ] })
        # Assign the output
        output[[ paste0("set_", i, "-emissions") ]] <- renderText({ customdata()$emissions[i] }) %>% bindEvent(input, { input_names() })
        output[[ paste0("set_", i, "-change") ]] <- renderText({ customdata()$change[i] }) %>% bindEvent(input, { input_names() })
      })
    }
  }) %>% bindEvent({ydata()})


  ## 3.3 STATISTICS ####################################################



  # DIAGNOSTICS ###############################


  # 5. OUTPUT #################################

  ## 5.1 QIs ##################################
  qi = reactive({
    req(ydata(), input$scenario_benchmark, input$scenario_yours, input$geoid, input$startyear, input$unit, input$pollutant)
    load("data/calculator/qi_scenario.rda")
    qi_scenario(input = input, ydata = ydata())
  })

  ## 5.2 DOWNLOAD #############################################
  output$download <- downloadHandler(
    contentType = "text/csv", filename = function(){"data.csv" },
    content = function(file){  qi()$data %>% select(-text) %>% write_csv(file) })


  ## 5.3 VISUAL ###############################
  output$visual = renderPlotly({
    req(qi())
    load("data/calculator/h_scenario.rda")
    h_scenario(qi())
  })

  ## 5.4 Equation =======================================
  output$equation = renderUI({ req(model()); print_equation(model())  })

  ### 5.5 Note Scaling ============================

  ## 5.6 GOF Stats =====================================
  observe({
    req(stats())  # When stats() updates, update all of these at once
    # Generate a key
    modelid = paste(input$geoid, input$pollutant, input$by, input$category, input$scaling, input$rounding, input$modeltype, input$modelrank, input$unit, sep = "-")
    if(nrow(stats()) > 0){
      output$sigma = renderText({ stats()$sigma }) %>% bindCache(modelid)
      output$percentrange = renderText({ stats()$percentrange }) %>% bindCache(modelid)
      output$yearrange = renderText({ stats()$yearrange }) %>% bindCache(modelid)
      output$nyears = renderText({  stats()$nyears }) %>% bindCache(modelid)
      output$yrange = renderText({  stats()$yrange }) %>% bindCache(modelid)
      output$yinterval = renderText({ stats()$yinterval }) %>% bindCache(modelid)
      output$accuracygauge = renderGauge({
        gauge(value = stats()$accuracygauge, min = 0, max = 100, symbol = "%", abbreviateDecimals = 1,
              sectors = gaugeSectors(success = c(95, 100), warning = c(90, 0.95), danger = c(0, 90)))
      }) %>% bindCache(modelid)
      # Add a note to the model (unrelated to GOF stats, but helpful to put it here.)
      output$note_scaling = renderText({
        print_note(yourformula = model()$yourformula, scaling = input$scaling, unit = input$unit)
      }) %>% bindCache(modelid)
    }
  }) %>% bindEvent({stats()})

  ## 5.7 Avg. Treatment Effect ============================
  output$stat = renderText({
    req(ydata(), input$unit)
    setcount = sets$count;
    if(setcount > 0){ stat = ydata() %>% filter(type == "custom") %>% with(change) %>% mean(na.rm = TRUE) %>%
      number(accuracy = 0.1, style_positive = "plus", style_negative = "minus", scale_cut = cut_si(unit = input$unit))
    }else{ stat = "..." }
    return(stat)
  }) %>% bindEvent({ydata(); input$unit})


  # Z. Diagnostics #####################################
  output$diagnostics <- renderPrint({ default() %>% glimpse()  })



}

#' @name shinyApp()
#' @description Invoke the shinyApp as a shinyapp object
shiny::shinyApp(ui = ui, server = server, onStart = global)



