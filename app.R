global = function(){

  library(dplyr)
  library(shiny)
  library(bslib)
  library(DT)
  devtools::load_all(".")
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
    selected = 98, choices = c("CO2e" = 98)
  )
  s8 = shiny::selectInput(
    inputId = "scenario", label = "SCENARIO",
    selected = "granddata.d36109", choices = c("Tompkins County, NY" = "granddata.d36109")
  )

  # Actual inputs
  s1 = shiny::selectInput(
    inputId = "by", label = "TYPE",
    choices = choices_aggregation,
    selected = "16")


  u1 = sidebar = bslib::sidebar(
    s9, s8, s1,
    open = TRUE)

  # Write a ui_dt mini-module
  #ui_dt = function(title, id){ DTOutput(outputId = id) }


  ui_dt = DT::DTOutput(outputId = "table")

  ui_plot = shiny::plotOutput(outputId = "visual")

  u2 = bslib::card(
    bslib::card_header("HIYA!"),
    bslib::card_body(ui_plot),
    bslib::card_body(ui_dt),
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
  # https://rstudio.github.io/DT/shiny.html
  # Set data-table page-length
  options(DT.options = list(pageLength = 5))

  render_dt = function(data, editable = "cell", server = TRUE, ...){
    renderDT(data, selection = 'none', server = server, editable = editable, ...)
  }

  # Generate input data from moveslite
  data = reactive({
    # Get data
    .scenario = input$scenario
    .pollutant = input$pollutant
    .by = input$by

    # Derive inputs
    .dbname = stringr::str_remove(.scenario, pattern = "[.].*")
    .table = stringr::str_remove(.scenario, pattern = paste0(.dbname, "[.]"))

    .byvalues = stringr::str_split(string = input$by, pattern = "[.]")
    .byid = .byvalues[[1]]
    # Build initial filter
    .filters = list(.pollutant = .pollutant, .by = .byid)
    # If there are 2 byvalues, grab the second; otherwise, leave it null
    if(length(.byvalues) > 1){
      .bytype = .byvalues[[2]]
      if(.byid == 14){ .filters$.fueltype = .bytype }
      if(.byid == 8){ .filters$.sourcetype = .bytype }
      if(.byid == 15){ .filters$.roadtype = .bytype }
      if(.byid == 12){ .filters$.regclass = .bytype }
    }

    db = connect(.dbname)
    data = moveslite::query_many(.db = db, .table = .table, .filters = .filters) %>%
      select(-any_of(c("geoid")))
    DBI::dbDisconnect(db)
    return(data)
  }) %>%
    bindEvent({input$scenario; input$pollutant; input$by})

  observe({
    output$table = render_dt(
      data = data(),
      editable = list(target = 'row', disable = list(columns = c(0,1))),
      rownames = FALSE)
  }) %>% bindEvent({ data() })

  observe({
    data <<- editData(data = data(), info = input$table_cell_edit, proxy = 'table', rownames = FALSE)
  }) %>% bindEvent({ input$table_cell_edit  })


  #proxy = dataTableProxy('table')


  output$visual = renderPlot({
    library(ggplot2)
    ggplot() +
      geom_line(data = data, mapping = aes(x = year, y = vmt))
  })
  #
#   output$visual = renderPlot({
#     library(ggplot2)
#
#     d = data()
#     ggplot() +
#       geom_line(data = d, mapping = aes(x = year, y = vmt))
#
#   }) %>% bindEvent({data()})

}

shiny::shinyApp(ui, server, onStart = global)
