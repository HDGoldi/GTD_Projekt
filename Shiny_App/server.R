library(shiny)
library(shinydashboard)
library(DT)
library(png)
library(DataExplorer)
library(treemap)
require(dplyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$data_explorer <- DT::renderDataTable({
        data_lite
    })
    
    # output$corrleation <- plot_correlation(data, type = 'continuous','iyear')
    
    
    output$raw_missing <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path(
            '../files',
            paste('raw_missing', input$n, '.png', sep =
                      '')
        ))
        
        # Return a list containing the filename
        list(src = filename)
    }, deleteFile = FALSE)
    
    
    output$lite_missing <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path(
            '../files',
            paste('missing_lite', input$n, '.png', sep =
                      '')
        ))
        
        # Return a list containing the filename
        list(src = filename)
    }, deleteFile = FALSE)
    
    output$killings1 <- renderPlot({
        treemap(
            dfk,
            #Your data frame object
            index = c("country", "iyear"),
            type = "value",
            vSize = "nkill",
            vColor = "nwound",
            palette = "RdBu",
            title = "Killings in Global terrorism  (Countries/Years) - size is proportional with the number of killings",
            title.legend = "Number of wounded",
            fontsize.title = 10
        )
    })
    
})
