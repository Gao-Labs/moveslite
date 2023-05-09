# Function to generate the UI for a single input set
generateInputSetUI <- function(setIndex) {
  ns <- NS(setIndex)
  tagList(
    fluidRow(
      column(3, textInput(ns("year"), "Year")),
      column(3, textInput(ns("vmt"), "VMT")),
      column(3, textInput(ns("vehicles"), "# Vehicles")),
      column(3, textInput(ns("sourcehours"), "Source hours")),
      column(3, textInput(ns("starts"), "Starts"))
    ),
    br()
  )
}

# Function to generate the server logic for a single input set
generateInputSetServer <- function(input, output, session) {
  # create reactive values for the current input set
  rv <- reactiveValues(year = "", vmt = "", vehicles = "", sourcehours = "", starts = "")
  
  observeEvent(input$year, {
    rv$year <- input$year
  })
  
  observeEvent(input$vmt, {
    rv$vmt <- input$vmt
  })
  
  observeEvent(input$vehicles, {
    rv$vehicles <- input$vehicles
  })
  
  observeEvent(input$sourcehours, {
    rv$sourcehours <- input$sourcehours
  })
  
  observeEvent(input$starts, {
    rv$starts <- input$starts
  })
  
  # return the reactive values for this input set
  return(rv)
}

# Function to generate the UI for all input sets
generateAllInputSetsUI <- function(numSets) {
  lapply(1:numSets, generateInputSetUI)
}

# Function to generate the server logic for all input sets
generateAllInputSetsServer <- function(input, output, session, numSets) {
  moduleList(
    paste0("set", 1:numSets) %>%
      set_names() %>%
      map(~callModule(generateInputSetServer, id = .x))
  )
}

# Function to add an input set
addInputSet <- function(numSets) {
  insertUI(
    selector = "#add_btn",
    where = "beforeBegin",
    ui = generateInputSetUI(numSets)
  )
}

# Function to remove an input set
removeInputSet <- function(setIndex) {
  removeUI(selector = paste0("#", setIndex))
}

# UI for app
ui <- fluidPage(
  fluidRow(
    column(2,
           actionButton(inputId = "add_btn", label = "Add Input Set"),
           br(),
           br(),
           actionButton(inputId = "remove_btn", label = "Remove Input Set")
    ),
    column(10,
           uiOutput("sets")
    )
  )
)
server <- function(input, output, session) {
  numSets <- reactiveValues(num = 1)
  
  # observe add button click
  observeEvent(input$add_btn, {
    numSets$num <- numSets$num + 1
    insertUI(
      selector = "#add_btn",
      where = "beforeBegin",
      ui = generateInputSetUI(numSets$num)
    )
    callModule(generateInputSetServer, id = numSets$num)
  })
  
  # observe remove button click
  observeEvent(input$remove_btn, {
    removeInputSet(paste0("set", numSets$num))
    numSets$num <- numSets$num - 1
  })
  
  # generate input sets
  modules <- callModule(generateAllInputSetsServer, id = "sets", numSets = numSets$num)
  
  # render input values
  output$input_values <- renderPrint({
    values <- reactiveValuesToList(modules)
    sapply(values, function(x) x$year)
  })
}

shinyApp(ui, server)