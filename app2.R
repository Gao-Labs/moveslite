library(dplyr)
library(DBI)
library(RSQLite)
library(shiny)
library(htmltools)
library(purrr)
require(plotly)
require(ggplot2)

rm(list = ls()); gc()

# Example inputs
# input = list(geoid = "36109", by = 16, pollutant = 98, modeltype = "best", startyear = 2023)
# sets = list()

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
      selectInput(inputId = "modeltype", label = "MODEL TYPE",choices = c("Simplest" = "simplest", "Best" = "best"), selected = "best", width = "100px"),
      selectInput(inputId = "geoid", label = "AREA", choices = c("Tompkins" = 36109), selected = 36109, width = "200px"),
      selectInput(inputId = "pollutant", label = "POLLUTANT", choices = c("CO2e" = 98), selected = 98, width = "200px"),
      selectInput(inputId = "by", label = "AGGREGATION", choices = c("Overall" = 16), selected = 16, width = "200px"),
      selectInput(inputId = "startyear", label = "START YEAR", choices = 1990:2060, 
                  selected = stringr::str_sub(Sys.Date(), 1,4), width = "200px")
    )
  ),
  mainPanel(
    fluidRow(
      div(id = "inputsets"),
      actionButton("add_set", "Add Input Set"),
      actionButton("remove_set", "Remove Input Set")
    ),
    fluidRow(
      verbatimTextOutput("input_values"),
      plotlyOutput("visual")
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
      "starts", "Vehicle Starts",
      "emissions", "Emissions",
      "change", "Change"
    ) %>%
      mutate(var = factor(var, levels = var))
    
    # Make an input text box for each predictor varaible available
    bundle_inputs = labeldata %>%
      filter(var %in% .vars) %>%
      split(.$var, drop = TRUE) %>%
      map(~column(textInput(inputId = ns(.$var), label = .$label, value = .data[[.$var]], width = "100px"), 
                  width = 2, offset = 0)) 
    
    
    .outcomes = c("emissions", "change")
    
    # Make an output textbox for each outcome metric
    bundle_outputs = labeldata %>%
      filter(var %in% .outcomes) %>%
      split(.$var, drop = TRUE) %>%
      map(~column(
        fluidRow(
          tags$b(.x$label, class = "text-output-label"),
          div(class = "text-output",textOutput(outputId = ns(.x$var)) )
        ),
        width = 2, offset = 0) ) 
    
    # Combine them
    result = append(bundle_inputs, bundle_outputs) %>% fluidRow(id = ns("inputset"))
    
    return(result)
  }
  
  
  #' @name present()
  #' @description Present supplied values as a data.frame
  #' @param i (Integer) Index of a given input set i (eg. set 1, 2, 3, 4)
  # present = function(i, vars = c("year", "vmt", "vehicles", 'sourcehours', "starts")){
  #   # Get namespace of input set
  #   ns <- NS(paste0("set", "_", i))
  #   # For as many variables as desired...
  #   # Extract columns of these variables
  #   vars %>% 
  #     # Bundle each into a tibble with proper names
  #     map(.f = ~{ tibble( input[[ ns(.x) ]] %>% as.numeric() ) %>% set_names(., nm = .x) }) %>% 
  #     # Keep just valid, non-empty data.frames
  #     keep(~nrow(.) > 0) %>%
  #     # And bind the columns together!
  #     bind_cols()
  # }
  
  # Initialize reactiveValues object to store input sets
  sets <- reactiveValues()
  sets$count <- 0
  
  # Get the range of years from selected start year (PRESENT) to the end of time
  observe({ sets$years = input$startyear:2060 }) %>% bindEvent({input$startyear})
  
  # Get a reactive values holder 'v'
  v = reactiveValues()
  
  
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
    model = estimate(data = default(), .vars = .vars, .check = FALSE)
    # Return and Print Message
    print("---model"); return(model)
  }) %>% 
    bindCache(input$geoid, input$pollutant, input$by, input$modeltype) %>%
    bindEvent({default()})
  
  
  # # Reactively find me all input names that start with outcome titles
  # input_names = reactive({
  #   input_nm = names(input);
  #   input_nm[grep("^year|^vmt|^vehicles|^sourcehours|^starts", input_nm)]
  # })
  
  
  # Update these outputs every time that a value changes...
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
      print("---stat()"); return(ydata)
    }
  })
  
  # 
  # # Get the names of TextOutputs containing "emissions"
  # outputboxes = reactive({ 
  #   list(
  #     emissions = names(output)[grepl("emissions", names(output), ignore.case = TRUE)],
  #     changes = names(output)[grepl("changes", names(output), ignore.case = TRUE)] 
  #   )
  # })
  # 
  # observe({
  #   req(default(), default_yearly(), ydata(), outputboxes())
  #   
  #   setcount = sets$count
  #   
  #   customdata = ydata() %>% filter(type == "custom") %>% arrange(year)
  #   
  #   # For each item...
  #   for(i in 1:length(outputboxes()$emissions) ){
  #     output[[ outputboxes()$emissions[i] ]] = renderText({ customdata$emissions[i] })
  #     print(outputboxes()$emissions[i])
  #   }
  #   
  #   for(i in 1:length(outputboxes()$changes) ){
  #     output[[ outputboxes()$changes[i] ]] = renderText({ customdata$changes[i] })
  #     print(outputboxes()$changes[i])
  #   }
  #   
  #   print("---outputboxes")
  # }) %>% bindEvent({ydata()})
  
  
  #%>% bindEvent({outputboxes()$emissions})
  
  observe({
    req(default(), default_yearly(), ydata())
    
    setcount = sets$count
    
    if(setcount > 0){
      
      customdata = ydata() %>% filter(type == "custom") %>% arrange(year)
      
      for (k in 1:setcount) {
        ns <- NS(paste0("set", "_", k))        
        ek = reactive({ customdata$emissions[k] })
        ec = reactive({ customdata$change[k] })
        output[[ns("emissions")]] <<- renderText({ ek() })
        output[[ns("change")]] <<- renderText({ ec() })
        remove(ns)
      }
    }
  }) %>% bindEvent({ ydata()  })
  
  # RENDER INPUT VALUES ###############################
  output$input_values <- renderPrint({ 
    ydata() %>%
      filter(type %in% c("benchmark", "custom"))
    }) %>% bindEvent({ydata()})
  
  
  output$visual = renderPlotly({
    
    req(ydata())
    
    benchmark = ydata() %>% filter(type %in% c("benchmark","pre_benchmark") ) %>% mutate(type = "Benchmark")
    custom = ydata() %>% filter(type == "custom")
    
    
    gg = ggplot() +
      geom_line(data = benchmark, 
                mapping = aes(x = year, y = emissions, group = type, color = type)) +
      geom_point(data = custom,
                 mapping = aes(x = year, y= emissions, color = type), size = 1.25) +
      geom_line(data = custom,
                mapping = aes(x = year, y= emissions, group = type, color = type))+
      geom_point(data = custom,
                 mapping = aes(x = year, y= emissions, color = type), size = 1.25) 
    
    pp = ggplotly( gg )
    
    return(pp)
  })
  
  
}


shinyApp(ui = ui, server = server)






