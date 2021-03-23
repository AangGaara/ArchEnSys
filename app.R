library(shiny)
library(shinythemes)
library(readxl)
library(networkD3)

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("EnArch",
                tabPanel("General Data","Blank"),
                tabPanel("Demand","Blank"),
                tabPanel("Supply","Blank"),
                tabPanel("Energy Resource","Blank"),
                tabPanel("Energy Flow","Blank"),
                tabPanel("Costs","Blank"),
                tabPanel("GHG Footprint","Blank")
                )
)

server <- function (input,output){
  output$txtout <- renderText ({paste(input@txt1,input$txt2, set = " ")})
}

shinyApp(ui = ui, server = server)