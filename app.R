#' @name app.R
#' @title CAT Calculator ShinyApp Script
#' @description User Interface and Server function (and supporting code) for running the calculator.

# Clear environment and cache
rm(list = ls()); gc()

# Load packages
library(readr)
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
# devtools::install_github("https://github.com/datalorax/equatiomatic")
# https://datalorax.github.io/equatiomatic/articles/colors.html
library(equatiomatic)
library(flexdashboard)
library(shinyWidgets)
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
set_theme = function(){
  require(ggplot2)
  theme_set(
    theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, margin = margin(0,0,0,0, "cm")),
        axis.ticks = element_blank(),
        axis.line = element_blank())
  )
}

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

  " %>%
  HTML()

js = '$(function() {
        $("[data-toggle=\'tooltip\']").tooltip({
          html: true,
          container: "body",
          sanitize: false
        });
      });'
#' @name tooltip()
#' @description Write a short function to operate a tooltip
tooltip = function(.title, ...){
  htmltools::tags$span(
    `data-toggle` = "tooltip", `data-placement` = "right",
    title = .title, ...)
}






#add this file and collapsible nature should work.

ui <- fluidPage(
  # Add head and styling
  tags$head(
    tags$style( mycss ),
    tags$script(HTML(js))
  ),
  # Add theme
  theme = bslib::bs_theme(version = 5, bootswatch = "cerulean"),
  shinyjs::useShinyjs(),
  # Add title

  titlePanel(
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
  ),


  # Layout Sidebar vs. Main Panel
  sidebarLayout(
    position = "left", fluid = TRUE,
    #read_rds("core.rds")$choices$search %>% head()


    sidebarPanel(
      width = 2,
      style = "max-width: 175px; min-width: 175x;",
      # Add your sidebar content here
      selectInput(inputId = "modeltype", label = "MODEL TYPE",choices = c("Simplest" = "simplest", "Best" = "best"), selected = "best", width = "100px"),
      selectInput(inputId = "geoid", label = "AREA", choices = read_rds("appdata.rds"), selected = 36109, width = "200px"),
      selectInput(inputId = "pollutant", label = "POLLUTANT",
                  choices = set_names(x = read_rds("core.rds")$choices$pollutant %>% as.numeric(),
                                      nm = read_rds("core.rds")$choices$pollutant %>% names()),
                  selected = 98, width = "200px"),
      selectInput(inputId = "by", label = "AGGREGATION",
                  choices = c("Overall" = 16,
                              "by Source" = 8,
                              "by Fuel Type" = 14,
                              "by Regulatory Class" = 12,
                              "by Road Type" = 15),
                  selected = 16, width = "200px"),
      selectInput(inputId = "category", label = "SUBTYPE",
                  choices = c("Overall" = 16),
                  selected = 16, width = "200px"),
      selectInput(inputId = "startyear", label = "START YEAR", choices = 1990:2060,
                  selected = stringr::str_sub(Sys.Date(), 1,4), width = "200px"),
      selectInput(inputId = "unit", label = "UNIT", choices = c("tons" = "t"), selected = "t", width = "200px")
    ),

    # In the main panel...
    mainPanel(
      width = 10,
      style = "max-width: 1200px; min-width: 300px; margin: 0 auto; padding-left: 0px; padding-top: 5px;",

      # TOOLBAR ###################################
      navs_tab_card(
        id = "toolbar",
        title = tags$b("MODEL"), selected = "metrics",
        nav(value = "metrics", title = "Metrics", icon = icon("weight-scale"),
            ## METRICS ###################################
            column_wrap(
              width = .25, gap = "2px",
              style = css(
                `padding-left` = "0px",
                `padding-right` = "0px",
                grid_template_columns = "1fr 1fr 1fr 1fr"),
              l = list(
                ### ACCURACY ####################################
                ui_card(
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
                ),

                ### AVERAGE ERROR ####################################
                ui_card(
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
                ),
                ### RANGE ####################################
                ui_card(
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
                ),
                ### YEARS ####################################
                ui_card(
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




              )
            )
        ),
        ## ABOUT ###################################
        nav(value = "model", title = "About this Model", icon = icon("magnifying-glass-chart"),
            card_body_fill(
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
                  uiOutput(outputId = "equation"))
              )
            )
        ),

        nav_item(
          dropdownButton(
            label = "More Features", icon = icon("person"),
            status = "primary", circle = FALSE,size = "sm",
            tooltip = tooltipOptions(title = "Click to Learn More!"),
              tags$b("SUBSCRIBE FOR MORE FEATURES!", style = "font-size: 14px;"),
              textInput(inputId = "email", label = "EMAIL", width = "200px",
                        placeholder = "youremail@gmail.com"),
              actionButton(inputId = "submit_email", label = "SUBMIT",
                           icon = icon("person"), width = "100px")
          )

        )
      ),
      # OUTPUT GRAPHICS ################################################
      fluidRow(
        # STATISTIC ###################################################
        column_wrap(
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
        )
      ),


      # INPUT SETS ################################################
      fluidRow(
        div(id = "inputsets"),
        # BUTTONS ################################################
        card(
          style = "border-color: transparent; padding: 2px; margin-bottom: 2px;",
          card_body(
            style = "padding: 5px; margin: 0px;",
            actionButton(inputId = "add_set", icon = icon("circle-plus"), label =  "Add Input Set"),
            actionButton(inputId = "remove_set", icon = icon("circle-minus"), label =  "Remove Input Set"))
        )
      )
    )
  )
)



server <- function(input, output, session) {

  # Set the ggplot theme one time.
  set_theme()
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

  ### UPDATE CATEGORY ##############################
  # Whenever you change the 'by' Aggregation field, the options for Category should change too.

  observe({
    req(input$by)
    .by = input$by
    # Update choices...
    .byname = switch(
      EXPR = .by,
      "16" = "overall",
      "8" = "sourcetype",
      "12" = "regclass",
      "14" = "fueltype",
      "15" = "roadtype")

    # Get ID
    .choices = read_rds("core.rds")$keywords %>%
      filter(type == .byname) %>%
      with(set_names(as.integer(.$id), .$term))

    updateSelectInput(inputId = "category", label = "SUBTYPE",
                      choices = .choices, selected = .choices[1])

  }) %>% bindEvent({ input$by })


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
      .vars = c("by", "sourcetype", "regclass", "fueltype", "roadtype", "year", "vmt", "vehicles", "starts", "sourcehours"))
    # Disconnect from database
    dbDisconnect(db); gc()
    # Return the data!
    print("---download()"); return(download)
  }) %>% bindCache(input$geoid, input$pollutant) %>% bindEvent({input$geoid; input$pollutant})

  # DEFAULT DATA #######################################
  default = reactive({
    # Require these before proceeding
    req(download(), input$modeltype, input$by, input$category)

    # Select just those variables from download()
    result = download() %>%
      # Filter to that aggregation level
      filter(by == input$by)

    # If Aggregation level is NOT overall, then narrow it further.
    if(input$by %in% c(8,12,14,15)){
      # Depending on the aggregation level, aggregate it by one variable or the other using input$category
      byname = switch(input$by, "8" = "sourcetype", "12" = "regclass", "14" = "fueltype", "15" = "roadtype")
      print(paste(input$by, "--", input$category))
      result = result %>% filter(!!sym(byname) == input$category)
    }

    # Get variables of interest depending on input$modeltype
    .vars = switch(EXPR = input$modeltype, "simplest" = c("year", "vmt"), "best" = c("year", "vmt", "vehicles", "sourcehours", "starts"))

    default = result %>%
      # Grab just variables; don't need geoid, pollutant, or by or categories anymore
      select(emissions, any_of(.vars))

    # Return and print completion message
    print("---default"); return(default)
  }) %>%
    bindCache(input$geoid, input$pollutant, input$by, input$category, input$modeltype) %>%
    # Trigger whenver download changes OR input$modeltype OR category changes.
    bindEvent({download(); input$modeltype; input$category })

  # DEFAULT YEARLY INTERPOLATED DATA #######################################
  default_yearly = reactive({
    # Require these variables before proceeding
    req(input$by, input$category, default(), sets$years )

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
    bindCache(input$geoid, input$pollutant, input$by, input$category, input$modeltype, input$startyear) %>%
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
    bindCache(input$geoid, input$pollutant, input$by, input$category, input$modeltype) %>%
    bindEvent({default()})

  # GOODNESS OF FIT ###########################################################
  stats = reactive({
    req(model())
    stats = model() %>% broom::glance() %>%
      # Extract quantities of interest
      select(accuracy = adj.r.squared, sigma, p_value = p.value, df, nobs) %>%
      # Format them
      mutate(
        # Get R2 as a number
        accuracygauge = round(accuracy*100, 1),
        p_value = round(p_value, 3),
        p_value = if_else(p_value == 0, true = "p < 0.001", false = paste0("p = ", p_value)))

    # Get year range
    stats$year_lower = model()$model$year %>% min(na.rm = TRUE)
    stats$year_upper = model()$model$year %>% max(na.rm = TRUE)
    # Get range of outcome variable
    stats$y_lower = model()$model$emissions %>% min(na.rm = TRUE)
    stats$y_upper = model()$model$emissions %>% max(na.rm = TRUE)

    stats = stats %>%
      mutate(yearrange = paste0(year_lower, " - ", year_upper),
             nyears = paste0(nobs, " cases"),
             # Get range
             yrange = abs(y_upper - y_lower),
             # Get percentage of range that sigma takes up
             percentrange = sigma / yrange,
             percentrange = percent(percentrange, accuracy = 0.1, suffix = "% of range"),
             sigma = number(sigma, accuracy = 0.01, scale_cut = cut_si(unit = input$unit)) %>% paste0("±", .),

             yrange = number(yrange, accuracy = 0.01, scale_cut = cut_si(unit = input$unit)),
             y_lower = number(y_lower, accuracy = 0.01, scale_cut = cut_si(unit = input$unit)),
             y_upper = number(y_upper, accuracy = 0.01, scale_cut = cut_si(unit = input$unit)),
             yinterval = paste0(y_lower, " - ", y_upper),

      )
    return(stats)
  })

  # OUTPUT GOODNESS OF FIT ####################################################
  #output$accuracy = renderText({ stats()$accuracy }) %>% bindEvent({ model() })
  output$accuracygauge = renderGauge({
    gauge(
      value = stats()$accuracygauge,
      min = 0, max = 100, symbol = "%",
      sectors = gaugeSectors(
        success = c(95, 100),
        warning = c(90, 0.95),
        danger = c(0, 90)
      ), abbreviateDecimals = 1)
  }) %>% bindEvent({model()})

  output$sigma = renderText({ stats()$sigma })
  output$percentrange = renderText({ stats()$percentrange })
  output$yearrange = renderText({ stats()$yearrange })
  output$nyears = renderText({ stats()$nyears })
  output$yrange = renderText({ stats()$yrange })
  output$yinterval = renderText({ stats()$yinterval })
  # Example
  # output$equation = renderUI({ withMathJax(helpText("Some math here $$\\alpha+\\beta$$")) })

  output$equation = renderUI({
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

    model() %>%
      extract_eq(
        use_coefs = TRUE, fix_signs = TRUE,
        wrap = TRUE,
        swap_var_names = var_names,
        var_colors = var_colors,
        coef_digits = 2) %>%
      withMathJax()
  })



  output$stat = renderText({
    setcount = sets$count;
    if(setcount > 0){
      stat =  ydata() %>% filter(type == "custom") %>% with(change) %>% mean(na.rm = TRUE) %>%
        number(accuracy = 0.1, style_positive = "plus", style_negative = "minus", scale_cut = cut_si(unit = input$unit))
    }else{ stat = "..." }
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

    req(ydata(), input$scenario_benchmark, input$scenario_yours, input$geoid, input$startyear, input$unit, input$pollutant)

    .scenario_name_benchmark = input$scenario_benchmark
    .scenario_name_yours = input$scenario_yours

    .highlight_color = "#2fa4e7"
      .geoid_label = read_rds("areas.rds") %>% filter(geoid == input$geoid) %>% with(label)

      .pollutant_info = read_rds("core.rds")$keywords %>%
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

      bridge = ydata() %>% filter(type == "pre_benchmark" & year == .prioryear) %>% mutate(type = "custom")


      .ydata = ydata() %>%
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
          "<b>", .scenario_name_benchmark, "</b>:", label_benchmark,
          "<br>",
          "<b>", .scenario_name_yours, "</b>:", label_custom,
          "<br>",
          "<b>Change</b>:", label_change, " (", label_percent, ")"))




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
          data = gaps,
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
        # scale_fill_manual(
        #   breaks = c("Benchmark" = "benchmark", "Your Scenario" = "custom"),
        #   name = "Scenario",
        #   guide = "none",
        #   values = c("grey", .highlight_color)) +
        scale_y_continuous(labels = scales::label_number(scale_cut = cut_si(.pollutant_unit_abbr))) +
        labs(y = paste0("Estimated Emissions\n(", .pollutant_unit, " of ", .pollutant_label, ")"),
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





