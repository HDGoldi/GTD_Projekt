library(shiny)
library(shinydashboard)
library(DT)
library(png)
library(DataExplorer)
library(treemap)
require(dplyr)
library(tm)
library(wordcloud)
library(shinyWidgets)
library(ggplot2)
library(leaflet)
library(plotly)


shinyServer(function(input, output, session) {
    data_lite <- read.csv("../gtd_lite2.csv")
    gtd <- read.csv("../gtd_lite2.csv")
    
    output$data_explorer <- DT::renderDataTable({
        data_lite
    })
    
    #data explorer UI
    output$yearSelection <- renderUI({
        sliderInput(
            "year",
            label = "Please select the period ",
            min =  min(gtd$iyear, na.rm = T),
            max = max(gtd$iyear, na.rm = T),
            value = c(min(gtd$iyear, na.rm = T), max(gtd$iyear, na.rm =
                                                         T)),
            sep = "",
            step = 1
        )
    })
    
    output$regionSelection <- renderUI({
        pickerInput(
            'region',
            label = 'Region',
            choices = unique(as.character(gtd$region)),
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = unique(gtd$region)
        )
    })
    
    output$countrySelection <- renderUI({
        pickerInput(
            'country',
            'Country',
            choices = unique(as.character(gtd$country)),
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = unique(gtd$country)
        )
    })
    
    output$attackSelection <- renderUI({
        pickerInput(
            'attack',
            label = 'Attack Type',
            choices = unique(as.character(gtd$attacktype)),
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = unique(gtd$attacktype)
        )
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
        gtd_sub <-
            subset(gtd_sub,
                   gtd_sub$iyear >= input$year[1] &
                       gtd_sub$iyear <= input$year[2])
        
        #removeModal()
    })
    
    
    observe({
        input$region
        sel <- unique(gtd[gtd$region == input$region, "country"])
        updateSelectInput(
            session = session,
            inputId = "country",
            choices = sel,
            selected = sel
        )
    })
    
    observe({
        input$tabitems
        print("Tab Event")
        
        updateSliderInput(
            session = session,
            inputId = "year",
            value = c(min(gtd$iyear, na.rm = T), max(gtd$iyear, na.rm = T))
        )
        updateSelectInput(
            session = session,
            inputId = "region",
            choices = unique(as.character(gtd$region)),
            selected = unique(gtd$region)
        )
        updateSelectInput(
            session = session,
            inputId = "country",
            choices = unique(as.character(gtd$country)),
            selected = unique(gtd$country)
        )
        updateSelectInput(
            session = session,
            inputId = "attack",
            choices = unique(as.character(gtd$attacktype)),
            selected = unique(gtd$attacktype)
        )
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
        plot_ly(
            d,
            x = d$region,
            type = "histogram",
            color = d$attacktype
        )
        #p <- ggplot(plot_gtdsub(), aes(x =.data$region, fill=.data$attacktype))+geom_histogram(stat= "count")
    })
    
    #Plot distribution by country top20
    output$dist_country <- renderPlotly({
        d <- plot_gtdsub()
        d <- d %>% count(d$country)
        d <- arrange(d, desc(n))
        d <- d[1:20,]
        p <-
            ggplot(d, aes(
                x = reorder(.data$'d$country', .data$n),
                y = .data$n
            )) +
            geom_bar(stat = "identity") + coord_flip()
        ggplotly(p)
    })
    
    #Plot distribution by tgroup top20
    output$dist_tgroup <- renderPlotly({
        d <- plot_gtdsub()
        d <- d %>% count(d$gname)
        d <- arrange(d, desc(n))
        d <- d[2:21,] #removing unknown groups
        p <-
            ggplot(d, aes(x = reorder(.data$`d$gname`, d$n), y = .data$n)) + geom_bar(stat = "identity") +
            coord_flip()
        ggplotly(p)
    })
    
    #Plot distribution by attacktype
    output$dist_attack <- renderPlotly({
        d <- plot_gtdsub()
        d <- d %>% count(d$attacktype)
        p <-
            ggplot(d, aes(x = .data$`d$attacktype`, y = .data$n)) + geom_bar(stat = "identity")
        p <-
            p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p)
    })
    
    #Plot distribution by weaptype
    output$dist_weap <- renderPlotly({
        d <- plot_gtdsub()
        d <- d %>% count(d$weaptype)
        p <-
            ggplot(d, aes(x = .data$`d$weaptype`, y = .data$n)) + geom_bar(stat = "identity")
        p <-
            p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p)
    })
    
    output$distyear <- renderPlotly({
        p1 <-
            ggplot(plot_gtdsub(), aes(x = .data$iyear)) + geom_histogram(stat = "count")
        ggplotly(p1)
    })
    
    output$distyear2 <- renderPlotly({
        p2 <-
            ggplot(plot_gtdsub(),
                   aes(x = .data$iyear, colour = .data$region)) + geom_freqpoly()
        ggplotly(p2)
    })
    
    output$attack_year <- renderValueBox({
        d <- plot_gtdsub()
        d <- d %>% count(d$iyear)
        
        sd <- round(sd(d$n), 1)
        
        valueBox(
            subtitle = paste("Mean Attacks per Year", " (SD: ", as.character(sd), ")"),
            value = round(mean(d$n), 1),
            icon = icon("stream")
        )
    })
    
    output$casual_att <- renderValueBox({
        d <- plot_gtdsub()
        d <- d %>% mutate(casualties = .data$nkill + .data$nwound)
        d <- d[!is.na(d$casualties),]
        
        sd <- round(sd(d$casualties), 1)
        
        valueBox(
            subtitle = paste(
                "Mean Casualties per Attack",
                " (SD: ",
                as.character(sd),
                ")"
            ),
            value = round(mean(d$casualties), 1) ,
            icon = icon("stream")
        )
    })
    
    
    output$map <- renderLeaflet({
        d <- plot_gtdsub()
        d <- d %>% mutate(casualties = .data$nkill + .data$nwound)
        d <-
            d[c('latitude',
                'longitude',
                'city',
                'country',
                'casualties')]
        dist_city <- distinct(d, d$city, .keep_all = TRUE)
        d2 <- d %>% count(city) %>% group_by(city)
        d2 <- d2 %>% arrange(desc(n))
        d2 <- d2[-grep("Unknown", d2$city),]
        d2 <- left_join(d2, dist_city, by = "city")
        d2 <- d2[!is.na(d2$latitude) & !is.na(d2$longitude),]
        d2 <-
            d2 %>% select(city, country, n, latitude, longitude) %>% rename(attackcount = n)
        d <-  d2[1:1000,]
        
        gtd_map <- d
        
        leaflet(data = gtd_map) %>%
            addTiles() %>%
            #addMarkers(lng = gtd_map$longitude,lat = gtd_map$latitude)
            addCircleMarkers(
                lng = gtd_map$longitude,
                lat = gtd_map$latitude,
                popup = paste(
                    sep = "</br>",
                    paste(gtd_map$city, ",", gtd_map$country),
                    paste("Attacks:", gtd_map$attackcount)
                ),
                radius = sqrt(gtd_map$attackcount * 0.5)
            )
    })
    
    output$raw_missing <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path(
            '../files',
            paste('raw_missing', input$n, '.png', sep =
                      '')
        ))
        
        # Return a list containing the filename
        list(src = filename,
             width = 500,
             height = 500)
    }, deleteFile = FALSE)
    
    
    output$lite_missing <- renderImage({
        # When input$n is 1, filename is ./images/image1.jpeg
        filename <- normalizePath(file.path(
            '../files',
            paste('missing_lite', input$n, '.png', sep =
                      '')
        ))
        
        # Return a list containing the filename
        list(src = filename,
             width = 500,
             height = 500)
    }, deleteFile = FALSE)
    
    output$killings1 <- renderPlot({
        data_lite %>% filter(nkill > 0) -> dfk
        treemap(
            dfk,
            index = c("iyear"),
            vSize = "nkill",
            palette = "Reds",
            fontsize.title = 14
        )
    })
    
    output$killings2 <- renderPlot({
        data_lite %>% filter(nkill > 0) -> dfk
        treemap(
            dfk,
            index = c("country"),
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
    
    output$word_cloud1 <- renderPlot({
        df %>% filter(!is.na(summary)) -> dfn0
        dfn0 %>% filter(summary != "") -> dfn
        text <- sample(dfn$summary, nrow(dfn) / 100)
        myCorpus <- Corpus(VectorSource(text))
        #myCorpus = tm_map(myCorpus, content_transformer(tolower))
        # remove punctuation
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        # remove stopwords for English
        myCorpus = tm_map(myCorpus,
                          removeWords,
                          c(stopwords("english"), stopwords("SMART"), "the"))
        #create DTM
        myDtm = TermDocumentMatrix(myCorpus,
                                   control = list(minWordLength = 3))
        #Frequent Terms and Associations
        freqTerms <- findFreqTerms(myDtm, lowfreq = 1)
        m <- as.matrix(myDtm)
        # calculate the frequency of words
        v <- sort(rowSums(m), decreasing = TRUE)
        myNames <- names(v)
        d <- data.frame(word = myNames, freq = v)
        wctop <-
            wordcloud(d$word,
                      d$freq,
                      min.freq = 50,
                      colors = brewer.pal(9, "Set1"))
    })
    
    output$dendogram1 <- renderPlot({
        mydata.df <-
            as.data.frame(inspect(removeSparseTerms(myDtm, sparse = 0.99)))
        mydata.df.scale <- scale(mydata.df)
        d <-
            dist(mydata.df.scale, method = "euclidean") # distance matrix
        fit <- hclust(d, method = "ward.D")
        plot(
            fit,
            xaxt = 'n',
            yaxt = 'n',
            xlab = "Word clustering using ward.D method",
            ylab = "",
            main = "Cluster Dendogram for words used in summary description"
        ) # display dendogram?
        groups <- cutree(fit, k = 5) # cut tree into 5 clusters
        # draw dendogram with blue borders around the 5 clusters
        rect.hclust(fit, k = 5, border = "blue")
    })
    
    output$downloadData <- downloadHandler(
        filename = "data.csv",
        content = function(file) {
            write.csv(data_lite, file)
        }
    )
    
})