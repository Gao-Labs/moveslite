
library(purrr)
library(dplyr)
library(htmltools)
library(shiny)
library(shinyjs)
library(bslib)
library(plotly)
library(ggplot2)

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
    wrapper = NULL, full_screen = TRUE,
    style = css(
      padding = "0px", `margin-top` = paste0(margin[1], "px"), 
      `margin-bottom` = paste0(margin[2], "px")),
    ...)
  return(result)
}


years = seq(from = 1990, to = 2060, by = 1)

i_rows = selectInput(inputId = "rows", label = "INPUTS", choices = 1:length(years), 
                   selected = 1, multiple = FALSE, width = "100px")

i_geoid = selectInput(inputId = "geoid", label = 'AREA', choices = c(36109, 36007), selected = 36109, multiple = FALSE, width = "200px")

i_pollutant = selectInput(inputId = "pollutant", label = 'POLLUTANT', choices = c(98), selected = 98, multiple = FALSE, width = "200px")

i_aggregation = selectInput(inputId = "aggregation", label = 'AGGREGATION', choices = c("Overall" = 16, "Sourcetype" = 8), selected = 16, multiple = FALSE, width = "200px")

ui = fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "cerulean"),
  shinyjs::useShinyjs(),
  
  # Scenario Info
  ui_card(
    card_title("Scenario X"),
    
    card_body_fill(
      fluidRow(
      column(
        div(
          fluidRow(i_geoid),
          fluidRow(i_pollutant),
          fluidRow(i_aggregation),
          fluidRow(i_rows)),
        width = 4),
      
      column(uiOutput("predictors"), width = 8),
      )
    ),
    card_body_fill(
      fluidRow(
      column("STATISTIC", width = 4),
      column(plotlyOutput("visual", height = "300px"), width = 8)
      )
    )
  )
)


server = function(input, output, session){ 
  
  
  output$predictors = renderUI({
    
    vars = tribble(
      ~var,   ~label,   ~value,
      "year", "Year",   2025,
      "vmt",  "VMT",    10000,
      "vehicles", "Vehicles", 3000,
      "sourcehours", "Hours Driven", 50000,
      "starts", "Vehicle Starts", 30000
    ) %>%
      mutate(id = 1:n())
    
    i = as.integer(input$rows)

    boxes = function(i){
      
      vars %>%
        split(.$id) %>%
        map(~{
          if(.x$var == "year" & i > 1){ add = i - 1 }else{ add = 0 }
          textInput(inputId = paste0(.x$var, i), label = .x$label, value = .x$value + add)
          }) %>%
        column_wrap(width = 25) %>%
        fluidRow()
    }
    
    1:i %>% map(~boxes(.)) %>% return()
  
  }) %>% bindEvent({input$rows})
  

  source("R/analyze.R")
  visual = renderPlotly({
    
    .filters = c(.by = input$aggregation, .pollutant = input$pollutant)
    
    
    analyze(.geoid = input$geoid, .filters = .filters, .newx = )  
    
  })
}

shinyApp(ui, server)





