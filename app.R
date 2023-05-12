library(DBI)
library(shiny)
library(htmltools)
library(dplyr)
library(purrr)

mycss = "
  .text-output-label {
    margin-left: 15px;
  }
  
  .text-output { 
    border: 1px solid #ced4da; 
    background-color: #f5f5f5;
    border-radius: 4px;
    padding: 6px 12px;
    margin-top: 5px;
    margin-left: 15px;
    margin-right: 15px;
    margin-bottom: 15px;
    width: 100px;
    }" %>%
  HTML()


ui <- fluidPage(
  # Add head and styling
  tags$head( tags$style( mycss )),
  titlePanel("Dynamic Input Sets"),
  sidebarPanel(
    # Database Parameter Inputs
    fluidRow(
      selectInput(inputId = "modeltype", label = "MODEL TYPE", choices = c("Simplest", "Best"),  selected = "Best", width = "150px"),
      selectInput(inputId = "geoid", label = "AREA", choices = c("Tompkins" = 36109, "Broome" = 36007), selected = 36109, width = "150px"),
      selectInput(inputId = "pollutant", label = "POLLUTANT", choices = c("CO2e" = 98), selected = 98, width = "150px"),
      selectInput(inputId = "by", label = "AGGREGATION", choices = c("Overall" = 16, "By Sourcetype" = 8, "By Regulatory Class" = 12), selected = 16, width = "150px")
      #selectInput(inputId = "subtype", label = "SUBTYPE", choices = c("Overall" = 1), selected = 98, width = "150px")
    ),
    
  ),
  mainPanel(
    fluidRow(
      # Placeholder for inputboxes
      div(id = "input_boxes"),
      actionButton("add_set", "Add Input Set")
    ),
    # View the Data
    fluidRow(
      verbatimTextOutput("dataview", placeholder = TRUE)
    )
  )
)


server = function(input, output, session){
  
  # DOWNLOAD FROM DATABASE ##################################
  default = reactive({
    # Requires all before proceeding
    req(input$modeltype, input$geoid, input$pollutant, input$by)

    # Set the values
    .geoid = input$geoid
    .by = input$by
    .pollutant = input$pollutant
    .vars = switch(
      EXPR = input$modeltype,
      "Simplest" = c("year", "vmt"),
      "Best" = c("year", "vmt", "vehicles", "sourcehours", "starts"))    
    
    # Load functions  
    source("R/connect.R")
    source("R/table_exists.R")
    source("R/query.R")
    
    # Connect to data database
    db = connect(.type = "data")
    db %>% dbListTables()
    # Construct table
    .table = paste0("d", .geoid)
    # Example
    # .filters = c(.by = 16, .pollutant = 98)
    .filters = c(.by = .by, .pollutant = .pollutant)
    
    # Otherwise, continue
    # Query the table with the supplied filters
    data = db %>% query(.table = .table, .filters = .filters, .vars = .vars)
    
    # Disconnect from database
    dbDisconnect(db); gc()
    # Return the data!
    return(data)
    
  }) %>% bindEvent({input$geoid; input$pollutant; input$by; input$modeltype})
  
  # Let's write a ui function to generate an input set
  ui_generate_set <- function(.year = "", .default){
    
    # Grab the next 5 year period
    #if(.year == ""){ .year = Sys.Date() %>% lubridate::round_date(., unit = "5 years") %>% lubridate::year() }
    
    
    
    # Get variable names from default data
    .vars = .default %>% select(-any_of(c("geoid", "emissions"))) %>% names()
    
    
    label_data = tribble(
      ~var, ~label,
      "year", "Year",
      "vmt", "VMT",
      "vehicles", "Vehicles",
      "sourcehours", "Time Driven",
      "starts", "Vehicle Starts",
      "remove_set", "Remove Input",
      "emissions", "Emissions",
      "change", "Change"
    )
    
    # Get namespace
    ns <- NS(paste0("set", "-", .year))
    # Get a remove button
    bundle_button = label_data %>%
      filter(var == "remove_set") %>%
      split(.$var) %>%
      map(~column(
        actionButton(inputId = ns(.$var), label = .$label, icon = icon("minus"), width = "100px"),
        width = 3, offset = 0) )
    
    # Get inputs
    bundle_inputs = label_data %>% 
      filter(var %in% .vars) %>% 
      split(.$var) %>%
      map(~column(
        textInput(
          inputId = ns(.$var), label = .$label, width = "100px",
          # Get the value for that variable at the selected year
          value = .default[ which(.default$year == .year), ][[.$var]]), 
        width = 3, offset = 0))
    

    # Get outputs
    bundle_outputs = label_data %>%
      filter(var %in% c("emissions", "change")) %>%
      split(.$var) %>%
      map(~column(
        fluidRow(
          tags$b(.x$label, class = "text-output-label"),
          div(class = "text-output",textOutput(outputId = ns(.x$var)) )
        ),
        width = 3, offset = 0) )
    
    # Append these items into one list....
    bundle = append(bundle_inputs, bundle_outputs)
    bundle = append(bundle_button, bundle)
    # And make them items in the same row.
    fluidRow(bundle)
  }
  

  
  # ESTIMATE MODEL ##########################################
  model = reactive({
    # Require both before proceeding
    req(default(), vars())
    # Load function
    source("R/setx.R")
    source("R/estimate.R")
    # Get variables from default data
    .vars = default() %>% select(-geoid, -emissions) %>% names()
    # Estimate a model
    estimate(data = default(), .vars = .vars, .check = FALSE)
  }) %>% 
    # Trigger when default data from database changes
    bindEvent({default()})
  
  # MAKE PREDICTIONS #########################################
  stat = reactive({    
    source("R/project.R")
    project(m = model(), data = default(), 
            .newx = xdata(), 
            .cats = "year", .exclude = "geoid", .context = TRUE)
  }) %>% bindEvent({ model(); xdata() })
 
  
  # Update this whenever default data changes
  xdata = reactive({ tibble(year = 2020, vmt = rnorm(1, mean = 10000)) }) %>% 
    bindEvent({ default() })
  
  
  # Track how many sets of input boxes have been added
  sets = reactiveValues(
    count = 0,
    # Start at current year
    year = lubridate::year(lubridate::round_date(Sys.Date(), unit = "year")))
  
  # ADD 1 SET OF BOXES #################
  observe({
    req(default())
    # Increase count
    sets$count = sets$count + 1
    # Insert new set
    insertUI(
      selector = "#input_boxes",
      where = "afterEnd",
      ui = ui_generate_set(.year = sets$year + sets$count, .default = default())
    )
  }) %>% bindEvent({input$add_set})
  
  
  # Get names of inputs which match remove button
  # Reactively find me all input names that start with outcome titles
  remove_button_names = reactive({  input_nm = names(input); input_nm[grep("^remove_set", input_nm)] })
  
  
  # TEST VIEW OUTPUT DATA ###################################
  output$dataview = renderPrint({ inputState() })
  
  
}

#rm(list = ls())
#.default = tibble(year = c(2020, 2021, 2023), vmt = c(30, 31, 32))
#ui_generate_set(.year = 2022, .default)


# GOAL:
# 1. WHENEVER 1 OUT OF N BUTTONS ARE PRESSED, FIND THE ID OF THAT BUTTON.
# 2. THEN, REMOVE THE SET THAT CONTAINS THAT BUTTON.


shinyApp(ui, server)


observe({
  
  if(sets$count > 1){      
    
    ns <- NS(paste0("set", .year, "_"))
    
    # 
    removeUI(
      selector = paste0("#year", n$num_sets, "-label"),
      multiple = TRUE
    )
    removeUI(
      selector = paste0("..."),
      multiple = TRUE
    )
    # Then reduce the number of sets remaining by 1
    sets$count = sets$count - 1      
  }
  
}) %>%
  # Trigger when any of these remove buttons are triggered
  bindEvent(input, remove_button_names())





server <- function(input, output, session) {
  
  # Initialize reactiveValues object to store input sets
  input_sets <- reactiveValues()
  input_sets$count <- 0
  
  # Get a reactive values holder 'v'
  v = reactiveValues()
  
  # criteria
  # input$geoid = "36109"
  # input$pollutant = 98
  # input$by = 16
 
  
  
  # Add another input set ui to the input_sets reactive list object.
  observe({
    input_sets$count <- input_sets$count + 1
    setcount <- input_sets$count
    # As long as there is more than 0 sets....
    # Fill in the values using xdata()???
    bundle_inputs = ui_set_input(setcount = setcount)
    bundle_outputs = ui_set_output(setcount = setcount)
    
    input_sets[[paste0("set", setcount)]] = append(bundle_inputs, bundle_outputs) %>% fluidRow()
    
  }) %>% 
    # Trigger whenever "add_set" button is pressed
    bindEvent({input$add_set})
  
  
  # Reactively find me all input names that start with outcome titles
  input_names = reactive({  
    input_nm = names(input); 
    input_nm[grep("^year|^vmt|^vehicles|^sourcehours|^starts", input_nm)] 
  })

  
  
  #' @name present()
  #' @description Present supplied values as a data.frame
  #' @param i (Integer) Index of a given input set i (eg. set 1, 2, 3, 4)
  present = function(i, vars = c("year", "vmt", "vehicles", 'sourcehours', "starts")){
    # Get namespace of input set
    ns <- NS(paste0("set", i, "_"))
    # For as many variables as desired...
    # Extract columns of these variables
    vars %>% 
      # Bundle each into a tibble with proper names
      map(.f = ~{ tibble( input[[ ns(.x) ]]) %>% set_names(., nm = .x) }) %>% 
      # Keep just valid, non-empty data.frames
      keep(~nrow(.) > 0) %>%
      # And bind the columns together!
      bind_cols()
  }
  
  # Update these outputs every time that a value changes...
  xdata = reactive({
    # Get the number of input sets
    setcount = input_sets$count
    # For these variables of interest
    vars = c("year", "vmt", "vehicles", 'sourcehours', "starts")
    
    # Given at least 1 set of inputs...
    if(setcount > 0){
      # For each set...
      data = 1:setcount %>%
        # Get xvar data
        map_dfr(~present(., vars = vars))
      return(data)
      
    }
    # Run movesai, tentatively represented as this:
  }) %>%
    # Whenever one of these inputs changes...
    bindEvent(input, input_names())
  
  # Update ydata whenever xdata changes
  ydata = reactive({
    # Placeholder movesai function
    movesai = function(data){ 
      if(nrow(data)>0){ 
        data %>% 
          mutate(emissions = round(rnorm(1:n()),2), 
                 change = round(rnorm(1:n()),2))
        }else{ data } }
    # Run MOVESAI
    xdata() %>% movesai()
  }) %>% bindEvent(xdata())
  

  # Whenever ydata changes, update values of textOutput boxes
  observe({
    # Get the number of input sets
    setcount <- input_sets$count
    # Given at least 1 set of inputs...
    if(setcount > 0){
      # For each set of inputs
      1:setcount %>%
        # Run this function...
        map(~{
          # Get the namespace of the set...
          ns <- NS(paste0("set", .x, "_"))
          
          # Get the ith value of the emission vector and change vector in ydata()
          # Render this textoutput
          output[[ ns("emissions") ]] <- renderText({ ydata()$emissions[.x] })
          output[[ ns("change") ]] <- renderText({ ydata()$change[.x] })
        })
    }
  }) %>% bindEvent({ydata()})
    

  # # Let's write a reactive function that captures the data any time a cell changes.
  # data = reactive({
  #   # Whenever one of these input values change, record it.
  #   require(dplyr)
  #   require(purrr)
  #   
  #   # For each input set
  #   data = 1:input_sets$count %>% 
  #     # Present the values as a data.frame
  #     map_dfr(~present(., vars = c("year", "vmt", "vehicles", "sourcehours", "starts")))
  #   
  #   return(data)
  # }) 
  
  
  # Remove input set from input_sets reactive list object.
  observe({
    # If there is at least 1 input set active...
    if(input_sets$count > 0) {
      # Get the number of input sets
      setcount <- input_sets$count
      # Remove that input set
      input_sets[[paste0("set", setcount)]] <- NULL
      # Subtract 1 from the number of input sets, and record it.
      input_sets$count <- input_sets$count - 1
    }
  }) %>% 
    # Trigger whenever "remove_set" button is pressed
    bindEvent({input$remove_set})
  
  
  # RENDER INPUT SETS ################################
  output$sets <- renderUI({
    
    # For each input set
    1:input_sets$count %>% 
      # Grab that input set
      map(~input_sets[[paste0("set", .) ]] ) %>%
      # put them all together in one div()
      tags$div(id = "input_sets", .)
  })
  
  
  # RENDER INPUT VALUES ###############################
  output$dataview <- renderPrint({ bind_cols(xdata(), ydata()) }) 
}


shinyApp(ui = ui, server = server)



