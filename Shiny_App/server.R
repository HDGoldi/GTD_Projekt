
library(shiny)
library(shinydashboard)
library(DT)

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(leaflet)
library(plotly)
library(htmltools)

shinyServer(function(input, output, session) {
    
    gtd <- read.csv("./Data/gtd_lite2.csv")
    data_loaded <- FALSE
    #gtd_uk <- gtd[gtd$country == "United Kingdom",]
    #gtd_eu <- gtd[gtd$region == "Western Europe",]
    
    
    #data explorer UI
    output$yearSelection <- renderUI({
        sliderInput("year", label = "Please select the period ",
                    min =  min(gtd$iyear, na.rm=T),
                    max = max(gtd$iyear, na.rm=T),
                    value = c(min(gtd$iyear, na.rm=T), max(gtd$iyear, na.rm=T)),
                    sep = ""
        )
    })
    
    output$regionSelection <- renderUI({
        pickerInput('region', label = 'Region', choices = unique(as.character(gtd$region)), options = list(`actions-box` = TRUE), multiple = TRUE, selected = unique(gtd$region))
    })
    
    output$countrySelection <- renderUI({
        pickerInput('country', 'Country', choices = unique(as.character(gtd$country)),  options = list(`actions-box` = TRUE), multiple = TRUE, selected = unique(gtd$country))
    })
    
    output$attackSelection <- renderUI({
        pickerInput('attack', label = 'Attack Type', choices = unique(as.character(gtd$attacktype)), options = list(`actions-box` = TRUE), multiple = TRUE, selected = unique(gtd$attacktype))
    })
    
    
      
    plot_gtdsub <- reactive({
        #showModal(modalDialog("Loading Data...", size = "l"))
        
        #filter for selected regions
        gtd_sub <- gtd[gtd$region %in% input$region,]
        
        #filter for selected countries
        gtd_sub <- gtd_sub[gtd_sub$country %in% input$country,]
        
        #filter for selected attack types
        gtd_sub <- gtd_sub[gtd_sub$attacktype %in% input$attack,]
        
        #filter for selected time period
        gtd_sub <- subset(gtd_sub, gtd_sub$iyear >= input$year[1] &  gtd_sub$iyear <= input$year[2])
        
        #removeModal()
    })
    
   
    observe({
        input$region
        sel <- unique(gtd[gtd$region == input$region, "country"])
        updateSelectInput(session = session, inputId = "country", choices = sel, selected = sel)
    })

    observe({
        input$tabitems
        print("Tab Event")
        
        updateSliderInput(session = session, inputId = "year", value = c(min(gtd$iyear, na.rm=T), max(gtd$iyear, na.rm=T)))
        updateSelectInput(session = session, inputId = "region", choices = unique(as.character(gtd$region)), selected = unique(gtd$region))
        updateSelectInput(session = session, inputId = "country", choices = unique(as.character(gtd$country)), selected = unique(gtd$country))
        updateSelectInput(session = session, inputId = "attack", choices = unique(as.character(gtd$attacktype)), selected = unique(gtd$attacktype))
    })
    
    output$datatable <- DT::renderDataTable({
      plot_gtdsub()
    }) 
    
    #Plot distribution by region
    output$dist_region1 <- renderPlotly({
        d <- plot_gtdsub()
        plot_ly(d, x = d$region, type = "histogram")
    })    
    
    #Plot distribution by region and attack type
    output$dist_region2 <- renderPlotly({
      d <- plot_gtdsub()
      plot_ly(d, x = d$region, type = "histogram", color = d$attacktype)
      #p <- ggplot(plot_gtdsub(), aes(x =.data$region, fill=.data$attacktype))+geom_histogram(stat= "count")
    })
    
    #Plot distribution by country top20
    output$dist_country <- renderPlotly({
      d <- plot_gtdsub()
      d <- d %>% count(d$country)
      d <- arrange(d, desc(n))
      d <- d[1:20,]
      p <- ggplot(d, aes(x = reorder(.data$'d$country', .data$n), y = .data$n))+geom_bar(stat = "identity")+coord_flip()
      ggplotly(p)
    })
    
    #Plot distribution by tgroup top20
    output$dist_tgroup <- renderPlotly({
        d <- plot_gtdsub()
        d <- d %>% count(d$gname) 
        d <- arrange(d, desc(n))
        d <- d[2:21,] #removing unknown groups
        p <- ggplot(d, aes(x = reorder(.data$`d$gname`, d$n), y = .data$n))+geom_bar(stat = "identity")+coord_flip()
        ggplotly(p)
    })
    
    #Plot distribution by attacktype 
    output$dist_attack <- renderPlotly({
        d <- plot_gtdsub()
        d <- d %>% count(d$attacktype)
        p <- ggplot(d, aes(x = .data$`d$attacktype`, y = .data$n))+geom_bar(stat = "identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p)
    })
    
    #Plot distribution by weaptype
    output$dist_weap <- renderPlotly({
        d <- plot_gtdsub()
        d <- d %>% count(d$weaptype)
        p <- ggplot(d, aes(x = .data$`d$weaptype`, y = .data$n))+geom_bar(stat = "identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p)
    })
    
    output$distyear <- renderPlotly({
      p1 <- ggplot(plot_gtdsub(), aes(x = .data$iyear))+geom_histogram(stat= "count")
      ggplotly(p1)
    })
    
    output$distyear2 <- renderPlotly({
      p2 <- ggplot(plot_gtdsub(), aes(x= .data$iyear, colour=.data$region))+geom_freqpoly()
      ggplotly(p2)
    })
    
    
    output$attack_year <- renderValueBox({
        d <- plot_gtdsub()
        d <- d %>% count(d$iyear)
        
        sd <- round(sd(d$n),1)
        
        valueBox(
            subtitle = paste("Mean Attacks per Year"," (SD: ",as.character(sd),")"),
            value = round(mean(d$n),1),
            icon = icon("stream")
        )
    })
    
    output$casual_att <- renderValueBox({
        d <- plot_gtdsub()
        d <- d %>% mutate(casualties = .data$nkill + .data$nwound)
        d <- d[!is.na(d$casualties),]
        
        sd <- round(sd(d$casualties),1)
        
        valueBox(
            subtitle = paste("Mean Casualties per Attack"," (SD: ",as.character(sd),")"),
            value = round(mean(d$casualties),1) ,
            icon = icon("stream")
        )
    })
    
    output$map <- renderLeaflet({
      #gtd_map<-gtd_uk[!is.na(gtd_uk$latitude) & !is.na(gtd_uk$longitude),]
        gtd2 <- plot_gtdsub
        top100 <- gtd2[c('latitude', 'longitude', 'city', 'country')]
        dist_city <- distinct(top100, top100$city, .keep_all = TRUE)
        by_city <- top100 %>% count(city) %>% group_by(city)
        by_city <- by_city %>% arrange(desc(n))
        by_city <- by_city[-grep("Unknown", by_city$city),]
        by_city <- left_join(by_city, dist_city, by = "city")
        by_city <- by_city[!is.na(by_city$latitude) & !is.na(by_city$longitude),]
        by_city <- by_city %>% select(city, country, n, latitude, longitude) %>% rename(attackcount = n)
        top100 <-  by_city[1:100,]
      
        gtd_map <- top100
        
      leaflet(data = gtd_map) %>%
        addTiles() %>%
        #addMarkers(lng = gtd_map$longitude,lat = gtd_map$latitude)
        addCircleMarkers(lng = gtd_map$longitude,lat = gtd_map$latitude,  
                         popup= paste(gtd_map$city,',',gtd_map$country,' \n Attack Counts: ',as.character(gtd_map$attackcount), sep = '\n'),
                         radius = as.integer(gtd_map$attackcount)/150)
    })  
      
})
