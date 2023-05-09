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
    fluidRow(
      radioButtons(
        inputId = "modeltype", label = "MODEL TYPE", 
        choices = c("Simplest", "Best"), 
        selected = "Best", width = "100px"),
      selectInput(inputId = "geoid", label = "AREA", choices = c("Tompkins" = 36109), selected = 36109, width = "200px"),
      selectInput(inputId = "pollutant", label = "POLLUTANT", choices = c("CO2e" = 98), selected = 98, width = "200px"),
      selectInput(inputId = "by", label = "AGGREGATION", choices = c("Overall" = 16, selected = 16, width = "200px"))
    ),
    
  ),
  mainPanel(
    fluidRow(
      actionButton("add_set", "Add Input Set"),
      uiOutput("sets"),
      actionButton("remove_set", "Remove Input Set")
    ),
    fluidRow(
      verbatimTextOutput("input_values")
    )
    
  )
)



# Let's write a ui function to generate an input set
ui_input_generate <- function(setcount){
  require(shiny)
  require(htmltools)
  
  data = tribble(
    ~var, ~label,
    "year", "Year",
    "vmt", "VMT",
    "vehicles", "Vehicles",
    "sourcehours", "Time Driven",
    "starts", "Vehicle Starts"
  )
  
  # Get namespace
  ns <- NS(paste0("set", setcount, "_"))
  
  data %>% 
    split(.$var) %>%
    map(~column(textInput(inputId = ns(.$var), label = .$label, value = 0, width = "100px"), 
                width = 3, offset = 0)) 

}


ui_output_generate = function(setcount){
  
  ns = NS(paste0("set", setcount, "_"))  
  
  data = tribble(
    ~var, ~label,
    "emissions", "Emissions",
    "change", "Change"
  ) 
  
  data %>%
    split(.$var) %>%
    map(~column(
          fluidRow(
            tags$b(.x$label, class = "text-output-label"),
            div(class = "text-output",textOutput(outputId = ns(.x$var)) )
      ),
      width = 3, offset = 0) ) 
}



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
  
  # Select vector of variables
  vars = reactive({
    switch(
      EXPR = input$modeltype,
      "Simplest" = c("year", "vmt"),
      "Best" = c("year", "vmt", "vehicles", "sourcehours", "starts")
    )  
  }) %>% bindEvent({input$modeltype})
  
  # Pull default data from database
  default = reactive({
    # Requires all before proceeding
    req(vars(), input$geoid, input$pollutant, input$by)
    
    .geoid = input$geoid
    .by = input$by
    .pollutant = input$pollutant
    .vars = vars()
    
    # Example Inputs
    # .geoid = "36109"
    # .vars = c("vmt", "vehicles", "starts", "sourcehours", "year")
    # 
    # Load functions  
    source("R/connect.R")
    source("R/table_exists.R")
    source("R/query.R")
    
    # Connect to data database
    db = connect("data")
    # Construct table
    .table = paste0("d", .geoid)
    # Does table exist in this database?
    exist = table_exists(db, .table)
    
    # If table does not exists, stop.
    if(exist == FALSE){ stop("Error: Table does not exist in database.") }
    
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
    
  }) %>% bindEvent({input$geoid; input$pollutant; input$by; vars()})
  
  
  # Estimate the model
  model = reactive({
    source("R/setx.R")
    source("R/estimate.R")
    # Estimate a model
    estimate(data = default(), .vars = vars(), .check = FALSE)
  })
  

  stat = reactive({    
    source("R/project.R")
    stat = project(model(), data = default(), .newx = .newx, .cats = .cats, .exclude = .exclude, .context = .context)
    # .newx = tibble(year = 2020, vmt = 10000)
    # .forecast = TRUE
    # .outcome = 'emissions'
    # .cats = "year"
    # .exclude = "geoid"
    
      
  })
  
  
  
  # Add another input set ui to the input_sets reactive list object.
  observe({
    input_sets$count <- input_sets$count + 1
    setcount <- input_sets$count
    
    # As long as there is more than 0 sets....
    # Fill in the values using xdata()???
    
    
    bundle_inputs = ui_input_generate(setcount = setcount)
    bundle_outputs = ui_output_generate(setcount = setcount)
    
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
  output$input_values <- renderPrint({ bind_cols(xdata(), ydata()) }) 
}


shinyApp(ui = ui, server = server)



