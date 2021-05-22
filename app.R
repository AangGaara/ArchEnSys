library(shiny)
library(shinythemes)
library(readxl)
library(networkD3)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Data for Long Term Energy Forecast"),
  fluidRow(
    column(4,
           selectInput("Prov",
                       "Province:",
                       c("All,
                         unique(as.character(historical_data_")))
  )
)

server <- function (input,output){
  output$txtout <- renderText ({paste(input@txt1,input$txt2, set = " ")})
}

shinyApp(ui = ui, server = server)