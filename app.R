global = function(){

  library(dplyr)
  library(shiny)
  library(shinyWidgets)
  library(bslib)
  library(DBI)
  library(DT)
  library(plotly)
  library(ggplot2)
  devtools::load_all(".")
  source("z/app_functions.R")
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
}


ui = function(){

  # Testing values only
  # input = list(scenario = "granddata.d36109", pollutant = 98, by = "16")

  # Borrowed from outside this miniapp
  s9 = shiny::selectInput(
    inputId = "pollutant", label = "POLLUTANT",
    selected = 98, choices = c("CO2e" = 98, "Unsupported Pollutant" = 0)
  )
  s8 = shiny::selectInput(
    inputId = "scenario", label = "SCENARIO",
    selected = "granddata.d36109", choices = c("Tompkins County, NY" = "granddata.d36109",
                                               "Unavailable Place" = "granddata.dXXXXX")
  )

  # Actual inputs
  z1 = shiny::selectInput(
    inputId = "by", label = "TYPE",
    choices = choices_aggregation,
    selected = "16")

  z2 = shinyWidgets::pickerInput(
    inputId = "predictors", label = "PREDICTORS",
    choices = c("year", "vmt", "vehicles", "sourcehours", "starts"),
    selected = c("year", "vmt"),
    multiple = TRUE)

  z3 = shiny::selectInput(
    inputId = "startyear", label = "START YEAR",
    choices = seq(from = 1990, to = 2060, by = 5),
    selected = "2020")

  z4 = shiny::selectInput(
    inputId = "unit", label = "UNITS",
    choices = c("tons" = "t"), selected = "t")


  u1 = sidebar = bslib::sidebar(
    s9, s8, z1, z2, z3, z4,
    open = TRUE)

  # Write a ui_dt mini-module
  #ui_dt = function(title, id){ DTOutput(outputId = id) }


  ui_dt = DT::DTOutput(outputId = "table")

  ui_plot = shiny::plotOutput(outputId = "visual")

  ui_test = shiny::verbatimTextOutput(outputId = "test", placeholder = TRUE)

  u2 = bslib::card(
    bslib::card_header("HIYA!"),
    bslib::card_body(ui_plot),
    bslib::card_body(ui_dt),
    # Testing components only
    bslib::card_footer(ui_test),

    fill = TRUE)


  output = bslib::page_fluid(
    bslib::layout_sidebar(
      u2,
      sidebar = u1
    ),
    theme = bslib::bs_theme(preset = "cerulean")

  )

  return(output)
}

server = function(input, output, session){

  # 0. DT SETTINGS ##################################
  # https://rstudio.github.io/DT/shiny.html
  # Set data-table page-length
  options(DT.options = list(pageLength = 5))

  # Write a specialize render function for your data table
  render_dt = function(data, editable = "cell", server = TRUE, ...){
    renderDT(data, selection = 'none', server = server, editable = editable, ...)
  }

  # 1. DATA ############################################################

  # Generate default input data from moveslite
  default = reactive({ get_default(.scenario = input$scenario, .pollutant = input$pollutant, .by = input$by) }) %>%
    bindEvent({input$scenario; input$pollutant; input$by})
  # Get variables in the data
  vars = reactive({ get_vars(.default = default())
    }) %>% bindEvent({req(default(), input$scenario); req(default(), input$pollutant); req(default(), input$by)})
  # Update the list of available predictors, every time the variable names change
  observe({
    .vars = vars()     # Get variables
    # Update the picker with new variable options, setting year and vmt as the only predictor selected
    shinyWidgets::updatePickerInput(
      session = session, inputId = "predictors", selected = c("year", "vmt"),
      choices = .vars, clearOptions = TRUE)
  }) %>% bindEvent(vars())

  # Train model on defaults...
  model = reactive({  estimate(data = default(), .vars = input$predictors);
  }) %>% bindEvent({ default() })

  # Get summary statistics about the model
  stats = reactive({ find_stats(m = model(), .unit = "t") }) %>% bindEvent({model()})

  # We want to make a 'data()' reactive object
  # which will represent the data currently shown in the table.
  # It starts off as a direct copy of default
  data = reactive({ default() }) %>% bindEvent({default()})

  # Every time a table cell is edited, update the data()
  observe({
    data <<- editData(data = data(), info = input$table_cell_edit, proxy = 'table', rownames = FALSE)
  }) %>% bindEvent({ input$table_cell_edit  })



  # Get newx (the rows in data() different from default())
  newx = reactive({
    get_newx(.default = default(), .data = data())
  }) %>% bindEvent({ data() })


  # Estimate ydata!
  ydata = reactive({
    get_ydata(.model = model(), .default = default(), .newx = newx(),
              .cats = "year", .context = TRUE, .ci = 0.95)
  }) %>% bindEvent({model(); default(); newx()})

  # Generate quantities of interest for visualization
  qi = reactive({ qi_scenario(.ydata = ydata(), .pollutant = input$pollutant, .unit = input$unit, .startyear = input$startyear) }) %>%
    bindEvent({ ydata(); input$startyear; input$unit })




  # 2. TABLE ###############################################
  # Every time data() changes, re-render the table
  observe({
    # Get current data
    .data = data()
    n_rows = length(.data)
    # If there are any rows, re-render table
    if(n_rows > 0){
      # We want to allow people to change the values of the **input** activity measures, eg. vmt
      # but NOT the id field **year** (column 0) or the outcome field **emissions** (column 1)
      output$table = render_dt(
        data = data(),
        editable = list(target = 'row', disable = list(columns = c(0,1))),
        rownames = FALSE)
    }else{
      # If there are no rows, render this table:
      nope = tibble(Result = "Data is not available for this query.")
      output$table = render_dt(data = nope, rownames(FALSE))
    }
  }) %>% bindEvent({ data() })



  # Re-estimate the emissions column
  # data <<- editData(data = data(), )

  # model = estimate(data = data(), .vars = .vars)
  #
  # # Suppose we had some information about 1 or more variables for a custom scenario year
  # # You can change "year", "vmt", "vehicles", "sourcehours", "starts" as you want
  # .newx = list(year = 2023, vmt = 343926)
  # .newx = tibble(year = 2023, vmt = 343926)
  # .newx = tibble(year = c(2023:2024), vmt = c(23023023, 234023402))
  #
  # # Quantities of interest
  # qis = project(m = model, data = default, .newx = .newx, .context = FALSE)

  #proxy = dataTableProxy('table')

  # Every time data() changes, re-render the graph
  output$visual = renderPlotly({
    .data = qi()
    n_rows = length(.data$data)
    # If there are any rows, render visual
    if(n_rows > 0){
      gg = h_scenario(data = .data)
    }else{
      # If there are no rows, render this visual:
      gg = plotly::plot_ly()
    }
    # Return it
    gg
  }) %>% bindEvent({ qi() })


  # output$test = renderText({
  #   .model = model(); paste0(.model$coefficient, collapse = "; ")
  # }) %>% bindEvent({model()})


}

shiny::shinyApp(ui, server, onStart = global)
