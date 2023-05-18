#' @name app.R
#' @title CAT Calculator ShinyApp Script
#' @description User Interface and Server function (and supporting code) for running the calculator.

# Clear environment and cache
rm(list = ls()); gc()

# Load packages
library(dplyr)
library(DBI)
library(RSQLite)
library(shiny)
library(htmltools)
library(purrr)
require(plotly)
require(ggplot2)
library(tidyr)
library(scales)
library(bslib)

# Write a ui function for this module
column_wrap = function(l, width,  ...){
  layout = bslib::layout_column_wrap(width = width, ...)
  layout$children <- l
  # Add in extra style attributes here.
  #layout$attrbs$style <- paste(layout$attrbs$style, style)
  return(layout)
}
ui_card = function(..., margin = c(0, 0), id = ""){

  # Write an inline CSS class called 'hideslide'
  # we can use to turn off any slides that are NOT the activated slide.
  internalcss = tags$head(tags$style(".hideslide { display: none; }"))

  # Graphic
  result = card(
    # Card head settings
    internalcss, id = id,
    wrapper = NULL,
    style = css(
      padding = "0px", `margin-top` = paste0(margin[1], "px"),
      `margin-bottom` = paste0(margin[2], "px")),
    ...)
  return(result)
}
# Example inputs
# input = list(geoid = "36109", by = 16, pollutant = 98, modeltype = "best", startyear = 2023)
# sets = list()

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
    width: 175px;
    margin-right: 5px;
  }

  " %>%
  HTML()



ui <- fluidPage(
  # Add head and styling
  tags$head( tags$style( mycss )),
  # Add theme
  theme = bslib::bs_theme(version = 5, bootswatch = "cerulean"),
  shinyjs::useShinyjs(),
  # Add title
  titlePanel("Calculator"),

  # Layout Sidebar vs. Main Panel
  sidebarLayout(
    position = "left", fluid = TRUE,

    sidebarPanel(
      width = 2,
      style = "max-width: 175px; min-width: 175x;",
      # Add your sidebar content here
      selectInput(inputId = "modeltype", label = "MODEL TYPE",choices = c("Simplest" = "simplest", "Best" = "best"), selected = "best", width = "100px"),
      selectInput(inputId = "geoid", label = "AREA", choices = c("Tompkins" = 36109), selected = 36109, width = "200px"),
      selectInput(inputId = "pollutant", label = "POLLUTANT", choices = c("CO2e" = 98), selected = 98, width = "200px"),
      selectInput(inputId = "by", label = "AGGREGATION", choices = c("Overall" = 16), selected = 16, width = "200px"),
      selectInput(inputId = "startyear", label = "START YEAR", choices = 1990:2060,
                  selected = stringr::str_sub(Sys.Date(), 1,4), width = "200px")

    ),

    # In the main panel...
    mainPanel(
      width = 10,
      style = "max-width: 1200px; min-width: 300px; margin: 0 auto; padding-left: 0px; padding-top: 5px;",
      # Add a toprow
      column_wrap(
        width = .25, gap = "2px", style = "padding-left: 0px; padding-right: 0px;",
        l = list(
          # ACCURACY STATISTIC ################################
          ui_card(
            card_header("Accuracy"),
            card_body_fill(textOutput(outputId = "accuracy", container = tags$h5)),
            style = "max-height: 100%;"),
          card("About this Model"),
          card("Box3"),
          card("Box4") )  ),
      # INPUT SETS ################################################
      fluidRow(
        div(id = "inputsets"),
        # BUTTONS ################################################
        card(
          style = "border-color: transparent; padding: 2px; margin-bottom: 2px;",
          card_body(
            style = "padding: 5px; margin: 0px;",
            actionButton("add_set", "Add Input Set"),
            actionButton("remove_set", "Remove Input Set"))
        )
      ),
      # OUTPUT GRAPHICS ################################################
      fluidRow(
        # STATISTIC ###################################################
        column(
          width = 3, offset = 0,
          ui_card(card_header("Average Change in Emissions"),
               card_body_fill(textOutput(outputId = "stat", container = tags$h5)),
                              style = "max-height: 100%;")
        ),
        # PLOT ###################################################
        column(
          width = 9, offset = 0,
          #verbatimTextOutput("input_values"),
          card(plotlyOutput("visual"))
        )
      )

    )

  )
)





server <- function(input, output, session) {

  # Load functions
  source("R/connect.R")
  source("R/query.R")
  source("R/setx.R")
  source("R/estimate.R")
  source("R/project.R")

  # Load this background data
  db = connect("cov")
  cov = db %>% tbl("areas") %>% filter(level %in% c("county", "state", "nation")) %>% collect()
  dbDisconnect(db); remove(db)

  #' @name ui_set()
  #' @description Function to generate a set of text inputs and output boxes
  #' @param setcount index number of the set to be produced
  #' @param data data.frame of default annual interpolated data points to be entered as default inputs.
  # Let's write a ui function to generate an input set
  ui_set <- function(setcount, data){

    # Get namespace
    ns <- NS(paste0("set", "_", setcount))

    # Get the year of that setcount
    .year = data$year[setcount]

    # Get default data pertaining to that year
    .data = data %>% filter(year == .year)

    # Get available predictor variable names
    .vars = .data %>% select(-any_of(c("emissions", "geoid"))) %>% names()
    # .vars = c("year", "vmt", "vehicles", "sourcehours", "starts")

    # Get labels
    labeldata = tribble(
      ~var, ~label,
      "year", "Year",
      "vmt", "VMT",
      "vehicles", "Vehicles",
      "sourcehours", "Time Driven",
      "starts", "Starts",
      "emissions", "Emissions",
      "change", "Change"
    ) %>%
      mutate(var = factor(var, levels = var))

    # Make an input text box for each predictor varaible available
    bundle_inputs = labeldata %>%
      filter(var %in% .vars) %>%
      split(.$var, drop = TRUE) %>%
      map(~ui_card(
        style = "padding-left: 5px; border-color: transparent;",
        textInput(inputId = ns(.$var), label = .$label, value = .data[[.$var]], width = "100px") ))


    .outcomes = c("emissions", "change")

    # Make an output textbox for each outcome metric
    bundle_outputs = labeldata %>%
      filter(var %in% .outcomes) %>%
      split(.$var, drop = TRUE) %>%
      map(~ui_card(
        style = "padding-left: 15px; padding-right: 0px; overflow: hidden; border-color: transparent;",
        fluidRow(
          tags$span(.x$label, class = "text-output-label"),
          div(
            class = "text-output",
            # Label each as, for example, set_1-emissions_1, set_2-emissions_2
            textOutput(outputId = ns(.x$var))) )  )   )

    # Combine them
    result = append(bundle_inputs, bundle_outputs) %>%
      column_wrap(
        l = ., width = 1/7,
        gap = "2px", style = "padding-left: 0px; padding-right: 0px;") %>%
      fluidRow(id = ns("inputset"), style = "padding-right: 0px;")

    return(result)
  }


  # Initialize reactiveValues object to store input sets
  sets <- reactiveValues()
  sets$count <- 0

  # Get the range of years from selected start year (PRESENT) to the end of time
  observe({ sets$years = input$startyear:2060 }) %>% bindEvent({input$startyear})

  # DOWNLOAD FROM DB ###################################
  download = reactive({
    # Requires all before proceeding
    req(input$geoid, input$pollutant)
    # Set values
    .geoid = input$geoid
    .pollutant = input$pollutant
    # Load functions
    # source("R/connect.R")
    # source("R/query.R")
    # Connect to data database
    db = connect("data")
    # Construct table
    .table = paste0("d", .geoid)
    # Query the table with the supplied filters
    download = query(
      .db = db, .table = .table,
      .filters = c(.pollutant = .pollutant),
      .vars = c("by", "year", "vmt", "vehicles", "starts", "sourcehours"))
    # Disconnect from database
    dbDisconnect(db); gc()
    # Return the data!
    print("---download()"); return(download)
  }) %>% bindCache(input$geoid, input$pollutant) %>% bindEvent({input$geoid; input$pollutant})

  # DEFAULT DATA #######################################
  default = reactive({
    # Require these before proceeding
    req(download(), input$modeltype, input$by)
    # Select just those variables from download()
    result = download() %>%
      # Filter to that aggregation level
      filter(by == input$by)


    # Get variables of interest depending on input$modeltype
    .vars = switch(EXPR = input$modeltype, "simplest" = c("year", "vmt"), "best" = c("year", "vmt", "vehicles", "sourcehours", "starts"))

    default = result %>%
      # Grab just variables; don't need geoid, pollutant, or by anymore
      select(emissions, any_of(.vars))

    # Return and print completion message
    print("---default"); return(default)
  }) %>%
    bindCache(input$geoid, input$pollutant, input$by, input$modeltype) %>%
    # Trigger whenver download changes OR input$modeltype
    bindEvent({download(); input$modeltype; input$by})

  # DEFAULT YEARLY INTERPOLATED DATA #######################################
  default_yearly = reactive({
    # Require these variables before proceeding
    req(default(), sets$years )
    # Load functions
    # source("R/setx.R")
    # Estimate for me each year-by-year, plus the previous years
    default_yearly = setx(
      data = default(),
      # Supply JUST a set of custom years
      .newx = tibble(year = sets$years ),
      .cats = "year", .context = FALSE) %>%
      # Drop the custom field - we don't need it yet
      filter(type == "benchmark") %>% select(-type)

    # Return the output
    print("---default_yearly()"); return(default_yearly)
  }) %>%
    bindCache(input$geoid, input$pollutant, input$by, input$modeltype, input$startyear) %>%
    # Trigger whenver download changes OR input$modeltype
    bindEvent({default(); sets$years })




  # ADD INPUT SET #####################################
  observe({
    # Require both before proceeding
    req(default_yearly(), input$add_set)
    # Update the counter
    sets$count <- sets$count + 1
    setcount <- sets$count
    # As long as there is more than 0 sets....
    # Fill in the values using default data
    #sets[[paste0("set", setcount)]] =
    insertUI(
      selector = "#inputsets",
      where = "beforeBegin",
      ui = ui_set(setcount = setcount, data = default_yearly()) )

  }) %>% bindEvent({input$add_set })


  # REMOVE INPUT SETS ################################
  observe({
    # If there is at least 1 input set active...
    if (sets$count > 0) {
      # Get the number of input sets
      setcount <- sets$count
      # Get the namespace
      ns <- NS(paste0("set", "_", setcount))
      # Remove the input set
      removeUI(selector = paste0("#", ns("inputset")), multiple = TRUE)
      # Subtract 1 from the number of input sets
      sets$count <- sets$count - 1
    }
  }) %>% bindEvent({input$remove_set})


  # MODEL ###############################
  # Estimate the model based on default data (NOT default interpolated yearly data)
  model = reactive({
    # Requires value default data before proceeding
    req(default())
    # source("R/estimate.R")
    # Using these predictor variables
    .vars = default() %>% select(-any_of(c("geoid", "emissions"))) %>% names()
    # Estimate this model
    model = estimate(data = default(), .vars = .vars)
    # Return and Print Message
    print("---model"); return(model)
  }) %>%
    bindCache(input$geoid, input$pollutant, input$by, input$modeltype) %>%
    bindEvent({default()})

  # GOODNESS OF FIT ###########################################################
  stats = reactive({
    req(model())
    setcount = sets$count
    if(setcount > 0){
      stats = model() %>% broom::glance() %>%
        # Extract quantities of interest
        select(accuracy = r.squared, sigma, p_value = p.value, df, nobs) %>%
        # Format them
        mutate(accuracy = percent(accuracy, accuracy = 1, suffix = "%"),
               sigma = number(sigma, accuracy = 1, scale_cut = cut_si(unit = "")),
               p_value = round(p_value, 3),
               p_value = if_else(p_value == 0, true = "p < 0.001", false = paste0("p = ", p_value)))
    }else{
      stats = tibble(accuracy = "", sigma = "",  p_value = "", df = "",nobs = "" )
    }
  })

  # OUTPUT GOODNESS OF FIT ####################################################
  output$accuracy = renderText({ stats()$accuracy }) %>% bindEvent({ model() })


  output$stat = renderText({
    setcount = sets$count;
    if(setcount > 0){
      stat =  ydata() %>% filter(type == "custom") %>% with(change) %>% mean(na.rm = TRUE) %>%
        number(accuracy = 1, style_positive = "plus", style_negative = "minus", scale_cut = cut_si(unit = ""))
    }else{ stat = "" }
    return(stat)
  })

  # UPDATE PREDICTIONS ########################################
  ydata = reactive({
    # Examples
    # sets = list(count = 1)
    # input = list("set_1-year" = "20233", "set_1-vmt" = "20224")
    # Only proceed after default_yearly() has been calculated
    req( default(), default_yearly(), model())

    # Get the number of input sets
    setcount = sets$count
    # For these variables of interest
    vars = c("year", "vmt", "vehicles", 'sourcehours', "starts")

    # Given at least 1 set of inputs...
    if(setcount > 0){

      # Don't continue until at least the first has shown up
      ns <- NS(paste0("set", "_", 1))
      req(input[[ ns("year") ]])

      # For each set...
      xdata = 1:setcount %>%
        # Get xvar data
        map_dfr(.f = ~{

          # For these variables of interest
          vars = c("year", "vmt", "vehicles", 'sourcehours', "starts")

          # Get namespace of input set
          ns <- NS(paste0("set", "_", .x))


          # For as many variables as desired...
          # Extract columns of these variables
          vars %>%
            # Bundle each into a tibble with proper names
            map(.f = ~{ tibble( input[[ ns(.x) ]] %>% as.numeric() ) %>% set_names(., nm = .x) }) %>%
            # Keep just valid, non-empty data.frames
            keep(~nrow(.) > 0) %>%
            # And bind the columns together!
            bind_cols()  })


      # Generate statistics
      ydata = project(
        m = model(), data = default(), .newx = xdata,
        .cats = "year", .exclude = "geoid", .context = TRUE)

      # Calculate change in emissions from default
      ydata = ydata %>%
        left_join(
          by = "year",
          y = default_yearly() %>% select(year, default = emissions)) %>%
        mutate(change = emissions - default)

      # Whenever xdata() changes, update the predictions
      print("---ydata()"); return(ydata)
    }
  })


  # RENDER OUTPUT VALUES ###################################################################
  observe({
    #req(default(), default_yearly(), ydata())
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
          input_names = reactive({ names(input)[ stringr::str_detect(names(input), pattern = paste0("set_", i)) ] })
          # Assign the output
          output[[ paste0("set_", i, "-emissions") ]] <- renderText({ customdata()$emissions[i] }) %>% bindEvent(input, { input_names() })
          output[[ paste0("set_", i, "-change") ]] <- renderText({ customdata()$change[i] }) %>% bindEvent(input, { input_names() })
        })

    }
  }) %>% bindEvent({ydata()})


  # RENDER INPUT VALUES ###############################
  output$input_values <- renderPrint({
    ydata() %>%
      filter(type %in% c("custom")) %>%
      select(year, emissions, default, change)
  }) %>% bindEvent({ydata()})

  # RENDER VISUAL #################################
  output$visual = renderPlotly({

    req(ydata())

    .geoid_label = "Tompkins County"
    .pollutant_label = "CO2 Equivalent"
    .pollutant_unit = "tons"
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

    bridge = ydata() %>% filter(type == "pre_benchmark" & year == .prioryear) %>% mutate(type = "custom")


    .ydata = ydata() %>%
      filter(type %in% c("custom", "benchmark", "pre_benchmark", "post_benchmark")) %>%
      # Join in the bridge year
      bind_rows(bridge) %>%
      # Mutate labels
      mutate(type = case_when(type == "custom" ~ "custom",
                              type != "custom" ~ "benchmark"),
             label_type = factor(type, levels = c("benchmark", "custom"),
                                 labels = c("Benchmark Scenario", "Your Scenario"))) %>%
      mutate(label_emissions = scales::number(emissions, accuracy = .1, scale_cut = scales::cut_si(unit = paste0(" ", .pollutant_unit)))) %>%
      mutate(text = paste0(
        "<b>Type</b>: ", label_type,
        "<br>",
        "<b>Year</b>: ", year,
        "<br>",
        "<b>Emissions</b>: ", label_emissions))

    benchmark = .ydata %>% filter(type == "benchmark")
    custom = .ydata %>% filter(type == "custom")

    # Get the bridge from the previous year

    # Example
    # tibble(type = c("benchmark", "custom"),
    #        year = c(2022, 2022),
    #        emissions = c(345, 263))%>%
    #   select(type, year, emissions) %>%
    #   pivot_wider(id_cols = c(year), names_from = type, values_from = emissions) %>%
    #   mutate(change = custom - benchmark)
    #
    gaps = bind_rows(benchmark, custom) %>%
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
        "<b>Benchmark</b>:", label_benchmark,
        "<br>",
        "<b>Your Scenario</b>:", label_custom,
        "<br>",
        "<b>Change</b>:", label_change, " (", label_percent, ")"))

    gg = ggplot() +
      geom_line(
        data = benchmark,
        mapping = aes(x = year, y = emissions, group = type, color = type, text = text)) %>%
      suppressWarnings() +
      geom_point(
        data = benchmark,
        mapping = aes(x = year, y= emissions, color = type, text = text), size = 1.25) %>%
      suppressWarnings() +
      geom_line(
        data = custom,
        mapping = aes(x = year, y= emissions, group = type, color = type, text = text)) %>%
      suppressWarnings() +
      geom_point(
        data = custom,
        mapping = aes(x = year, y= emissions, color = type, text = text), size = 1.25)  %>%
      suppressWarnings() +
      geom_linerange(
        data = gaps,
        mapping = aes(x = year, ymin = ymin, ymax = ymax, text = text)) %>%
      suppressWarnings() +

      scale_color_manual(
        breaks = c("Benchmark" = "benchmark", "Your Scenario" = "custom"),
        name = "Scenario",
        values = c("grey", "red")) +
      scale_y_continuous(labels = scales::label_number(scale_cut = cut_si(.pollutant_unit_abbr))) +
      labs(y = paste0("Estimated Emissions (", .pollutant_unit, " of ", .pollutant_label, ")"),
           x = "Year",
           title = paste0("Projected Emissions over Time in ", .pollutant_label)) +
      theme(legend.position = "none")

    pp = ggplotly( gg, tooltip = "text")

    # Update configuration
    pp = pp %>%
      layout(hoverlabel = list(align = "left")) %>%
      config(displayModeBar = FALSE, displaylogo = FALSE)
      # config(modeBarButtonsToRemove = list(
      #   "sendDataToCloud", "zoom2d", "pan2d", "select2d", "lasso2d",
      #   "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
      #   "hoverClosestCartesian", "hoverCompareCartesian",
      #   "zoom3d", "pan3d", "orbitRotation", "tableRotation",
      #   "handleDrag3d", "resetCameraDefault3d", "resetCameraLastSave3d",
      #   "hoverClosest3d", "zoomInGeo", "zoomOutGeo", "resetGeo",
      #   "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
      #   "toggleHover", "resetViews", "toImage", "toggleSpikelines",
      #   "resetViewMapbox")
      # )


    return(pp)
  })


}


shinyApp(ui = ui, server = server)






