
library(shiny)
library(shinydashboard)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$datatable <- DT::renderDataTable({
        data
    })
    
   # output$corrleation <- plot_correlation(data, type = 'continuous','iyear')

})
