library(shiny)
library(shinydashboard)
library(DT)
library(png)
library(DataExplorer)
library(treemap)
require(dplyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    data_lite <- read.csv("../gtd_lite2.csv")
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
        data_lite %>% filter(nkill > 0) -> dfk
        treemap(dfk, 
                index=c("iyear"), 
                vSize = "nkill",  
                palette = "Reds",  
                fontsize.title = 14 
        )
    })
    
    output$killings2 <- renderPlot({
        data_lite %>% filter(nkill > 0) -> dfk
        treemap(dfk, 
                index=c("country"), 
                vSize = "nkill",  
                palette = "Reds",  
                fontsize.title = 14 
        )
    })
    
    output$killings3 <- renderPlot({
        data_lite %>% filter(nkill > 0) -> dfk
        treemap(
            dfk,
            #Your data frame object
            index = c("country", "iyear"),
            type = "value",
            vSize = "nkill",
            vColor = "nwound",
            palette = "RdBu",
            title.legend = "Number of wounded",
            fontsize.title = 10
        )
    })
    
    selectedData <- reactive({
        data_lite[, c(input$xcol, input$ycol)]
    })
    
    output$total_values <- renderValueBox({
        valueBox(
            subtitle = "Total Rows",
            value = nrow(data_lite),
            icon = icon("stream")
        )
    })
    
    output$total_years <- renderValueBox({
        df_uniq <- unique(data_lite$iyear)
        valueBox(
            subtitle = "Total Years",
            value = length(df_uniq),
            icon = icon("stream"),
            color = "light-blue",
        )
    })
    
    output$total_countries <- renderValueBox({
        df_uniq <- unique(data_lite$country)
        valueBox(
            subtitle = "Total Countries",
            value = length(df_uniq),
            icon = icon("stream"),
            color = "light-blue"
        )
    })
    output$total_cities <- renderValueBox({
        df_uniq <- unique(data_lite$city)
        valueBox(
            subtitle = "Total Cities",
            value = length(df_uniq),
            icon = icon("stream"),
            color = "light-blue"
        )
    })
    
    output$total_killed <- renderValueBox({
        dfk %>% summarise(nkills = sum(nkill)) -> dfyr
        valueBox(
            subtitle = "Total Killed",
            value = dfyr,
            icon = icon("stream"),
            color = "red"
        )
    })
    
    output$total_wounded <- renderValueBox({
        dfk %>% filter(nwound > 0) %>% summarise(nkills = sum(nwound)) -> dfyr
        valueBox(
            subtitle = "Total Wounded",
            value = dfyr,
            icon = icon("stream"),
            color = "orange"
        )
    })
    
})
