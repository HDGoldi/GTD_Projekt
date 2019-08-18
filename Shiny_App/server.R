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
library(DataExplorer)
library(shinyGlobe)
library(tsbox)


shinyServer(function(input, output, session) {
    data_lite <- readRDS("lite_data.rds")
    data <- readRDS("raw_data.rds")
    
    output$data_explorer <- DT::renderDataTable(data_lite,
                                                options = list(scrollX = TRUE))
    
    #data explorer UI
    output$yearSelection <- renderUI({
        sliderInput(
            "year",
            label = "Please select the period ",
            min =  min(data_lite$iyear, na.rm = T),
            max = max(data_lite$iyear, na.rm = T),
            value = c(
                min(data_lite$iyear, na.rm = T),
                max(data_lite$iyear, na.rm =
                        T)
            ),
            sep = "",
            step = 1
        )
    })
    
    output$regionSelection <- renderUI({
        pickerInput(
            'region',
            label = 'Region',
            choices = unique(as.character(data_lite$region)),
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = unique(data_lite$region)
        )
    })
    
    output$countrySelection <- renderUI({
        pickerInput(
            'country',
            'Country',
            choices = unique(as.character(data_lite$country)),
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = unique(data_lite$country)
        )
    })
    
    output$attackSelection <- renderUI({
        pickerInput(
            'attack',
            label = 'Attack Type',
            choices = unique(as.character(data_lite$attacktype)),
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = unique(data_lite$attacktype)
        )
    })
    
    plot_gtdsub <- reactive({
        #filter for selected regions
        gtd_sub <- data_lite[data_lite$region %in% input$region, ]
        
        #filter for selected countries
        gtd_sub <- gtd_sub[gtd_sub$country %in% input$country, ]
        
        #filter for selected attack types
        gtd_sub <- gtd_sub[gtd_sub$attacktype %in% input$attack, ]
        
        #filter for selected time period
        gtd_sub <-
            subset(gtd_sub,
                   gtd_sub$iyear >= input$year[1] &
                       gtd_sub$iyear <= input$year[2])
    })
    
    observe({
        input$region
        sel <-
            unique(data_lite[data_lite$region == input$region, "country"])
        updateSelectInput(
            session = session,
            inputId = "country",
            choices = sel,
            selected = sel
        )
    })
    
    observe({
        input$tabitems
        
        updateSliderInput(
            session = session,
            inputId = "year",
            value = c(
                min(data_lite$iyear, na.rm = T),
                max(data_lite$iyear, na.rm = T)
            )
        )
        updateSelectInput(
            session = session,
            inputId = "region",
            choices = unique(as.character(data_lite$region)),
            selected = unique(data_lite$region)
        )
        updateSelectInput(
            session = session,
            inputId = "country",
            choices = unique(as.character(data_lite$country)),
            selected = unique(data_lite$country)
        )
        updateSelectInput(
            session = session,
            inputId = "attack",
            choices = unique(as.character(data_lite$attacktype)),
            selected = unique(data_lite$attacktype)
        )
    })
    
    output$datatable <- DT::renderDataTable({
        plot_gtdsub()
    })
    
    
    #boxplots
    output$boxplot1 <- renderPlot({
        boxplot(data_lite$nkill, xlab = "nkill")
    })
    
    output$boxplot2 <- renderPlot({
        boxplot(data_lite$nkillter, xlab = "nkillter")
    })
    
    output$boxplot3 <- renderPlot({
        boxplot(data_lite$nwound, xlab = "nwound")
    })
    
    output$boxplot4 <- renderPlot({
        boxplot(data_lite$nwoundte, xlab = "nwoundte")
    })
    
    #boxplots w/o outliers
    output$boxplot5 <- renderPlot({
        boxplot(data_lite$nkill, outline = FALSE, xlab = "nkill w/o outliers")
    })
    
    output$boxplot6 <- renderPlot({
        boxplot(data_lite$nkillter,
                outline = FALSE,
                xlab = "nkillter w/o outliers")
    })
    
    output$boxplot7 <- renderPlot({
        boxplot(data_lite$nwound,
                outline = FALSE,
                xlab = "nwound w/o outliers")
    })
    
    output$boxplot8 <- renderPlot({
        boxplot(data_lite$nwoundte,
                outline = FALSE,
                xlab = "nwoundte w/o outliers")
    })
    
    #Shapiro-Test console output
    output$shapiro1 <- renderPrint({
        #create sample of 5000
        ds <- sample_n(data_lite, size = 5000, replace = TRUE)
        shapiro.test(ds$nkill)
    })
    output$shapiro2 <- renderPrint({
        #create sample of 5000
        ds <- sample_n(data_lite, size = 5000, replace = TRUE)
        shapiro.test(ds$nkillter)
    })
    output$shapiro3 <- renderPrint({
        #create sample of 5000
        ds <- sample_n(data_lite, size = 5000, replace = TRUE)
        shapiro.test(ds$nwound)
    })
    output$shapiro4 <- renderPrint({
        #create sample of 5000
        ds <- sample_n(data_lite, size = 5000, replace = TRUE)
        shapiro.test(ds$nwoundte)
    })
    
    # function to get the mode.
    getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    #Mode console output
    output$mode1 <- renderPrint({
        paste("Mode nkill:", getmode(data_lite$nkill))
    })
    output$mode2 <- renderPrint({
        paste("Mode nkillter:", getmode(data_lite$nkillter))
    })
    output$mode3 <- renderPrint({
        paste("Mode nwound:", getmode(data_lite$nwound))
    })
    output$mode4 <- renderPrint({
        paste("Mode nwoundter:", getmode(data_lite$nwoundte))
    })
    
    
    
    #Plot casualty distribution by year
    output$casdist_year <- renderPlotly({
        d <- plot_gtdsub()
        d <-
            d %>% mutate(casualties = ifelse(
                is.na(d$nkill),
                ifelse(is.na(d$nwound), 0, d$nwound),
                ifelse(is.na(d$nwound), d$nkill, d$nkill + d$nwound)
            ))
        d <-
            setNames(aggregate(
                d$casualties,
                by = list(d$iyear),
                sum,
                na.rm = T
            ),
            c("iyear", "casualties"))
        
        p1 <-
            ggplot(d, aes(x = .data$iyear, .data$casualties)) + geom_bar(stat = "identity")
        
        ggplotly(p1)
    })
    
    #Plot casualty distribution by region
    output$casdist_region <- renderPlotly({
        d <- plot_gtdsub()
        d <-
            d %>% mutate(casualties = ifelse(
                is.na(d$nkill),
                ifelse(is.na(d$nwound), 0, d$nwound),
                ifelse(is.na(d$nwound), d$nkill, d$nkill + d$nwound)
            ))
        d <-
            setNames(aggregate(
                d$casualties,
                by = list(d$region),
                sum,
                na.rm = T
            ),
            c("region", "casualties"))
        
        p1 <-
            ggplot(d, aes(x = .data$region, .data$casualties)) + geom_bar(stat = "identity")
        p1 <-
            p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p1)
    })
    
    #Plot casualty distribution by attacktype
    output$casdist_attack <- renderPlotly({
        d <- plot_gtdsub()
        d <-
            d %>% mutate(casualties = ifelse(
                is.na(d$nkill),
                ifelse(is.na(d$nwound), 0, d$nwound),
                ifelse(is.na(d$nwound), d$nkill, d$nkill + d$nwound)
            ))
        d <-
            setNames(
                aggregate(
                    d$casualties,
                    by = list(d$attacktype),
                    sum,
                    na.rm = T
                ),
                c("attacktype", "casualties")
            )
        
        p1 <-
            ggplot(d, aes(x = .data$attacktype, .data$casualties)) + geom_bar(stat = "identity")
        p1 <-
            p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p1)
    })
    
    #Plot casualty distribution by weaptype
    output$casdist_weap <- renderPlotly({
        d <- plot_gtdsub()
        d <-
            d %>% mutate(casualties = ifelse(
                is.na(d$nkill),
                ifelse(is.na(d$nwound), 0, d$nwound),
                ifelse(is.na(d$nwound), d$nkill, d$nkill + d$nwound)
            ))
        d <-
            setNames(
                aggregate(
                    d$casualties,
                    by = list(d$weaptype),
                    sum,
                    na.rm = T
                ),
                c("weaptype", "casualties")
            )
        
        p1 <-
            ggplot(d, aes(x = .data$weaptype, .data$casualties)) + geom_bar(stat = "identity")
        p1 <-
            p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p1)
    })
    
    #Plot attack distribution by year
    output$atdist_year <- renderPlotly({
        d <- plot_gtdsub() %>% rename(year = iyear)
        p <-
            ggplot(d, aes(x = year)) + geom_bar(stat = "count") + xlab("Year") + ylab("Attack Count")
        ggplotly(p)
    })
    
    #Plot attack distribution by region
    output$atdist_region <- renderPlotly({
        p <-
            ggplot(plot_gtdsub(), aes(x = region), colours()) + geom_bar(stat = "count") + xlab("Region") + ylab("Attack Count")
        p <-
            p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p, tooltip = c("y", "x"))
    })
    
    #Plot attack distribution by attacktype
    output$atdist_attack <- renderPlotly({
        d <- plot_gtdsub()
        d <-
            d %>% count(d$attacktype) %>% rename(attacktype = 'd$attacktype', count = 'n')
        p <-
            ggplot(d, aes(x = attacktype, y = count)) + geom_bar(stat = "identity") + xlab("Attack Type") + ylab("Attack Count")
        p <-
            p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p)
    })
    
    #Plot attack distribution by weaptype
    output$atdist_weap <- renderPlotly({
        d <- plot_gtdsub()
        d <-
            d %>% count(d$weaptype) %>% rename(weapontype = 'd$weaptype', count = 'n')
        p <-
            ggplot(d, aes(x = weapontype, y = count)) + geom_bar(stat = "identity") + xlab("Weapon Type") + ylab("Attack Count")
        p <-
            p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p)
    })
    
    #Placeholder for time series plot
    output$time_series <- renderPlotly({
        d <- data_lite
        d <-
            d %>% mutate(casualties = ifelse(
                is.na(d$nkill),
                ifelse(is.na(d$nwound), 0, d$nwound),
                ifelse(is.na(d$nwound), d$nkill, d$nkill + d$nwound)
            ))
        d <-
            d %>% group_by(iyear) %>% summarise(
                terrorist_attacks_count = n(),
                killed = sum(casualties, na.rm = TRUE)
            )
        
        newrow <-
            tibble(iyear = 1993,
                   terrorist_attacks_count = (5073 + 3458) / 2 - 0.5)
        
        d2 <- d2 %>% arrange(d2$iyear)
        d2 <- d %>% bind_rows(newrow)
        
        values <- d2[, 2:3]
        
        ts1 <- ts(values,
                  start = 1970,
                  end = 2016,
                  frequency = 1)
        
        ggplotly(ts_ggplot(ts1))
    })
    
    
    #Plot distribution by region and attack type
    output$dist_region2 <- renderPlotly({
        d <- plot_gtdsub()
        p <-
            ggplot(plot_gtdsub(),
                   aes(x = .data$region, fill = .data$attacktype)) + geom_histogram(stat = "count", position =
                                                                                        position_dodge())
        p <-
            p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        ggplotly(p)
    })
    
    #Plot distribution by country top20
    output$dist_country <- renderPlotly({
        d <- plot_gtdsub()
        d <- d %>% count(d$country)
        d <- arrange(d, desc(n))
        d <- d[1:20, ]
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
        d <-
            d %>% mutate(casualties = ifelse(
                is.na(d$nkill),
                ifelse(is.na(d$nwound), 0, d$nwound),
                ifelse(is.na(d$nwound), d$nkill, d$nkill + d$nwound)
            ))
        d <- d[!is.na(d$casualties), ]
        
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
        d <-
            d %>% mutate(casualties = ifelse(
                is.na(d$nkill),
                ifelse(is.na(d$nwound), 0, d$nwound),
                ifelse(is.na(d$nwound), d$nkill, d$nkill + d$nwound)
            ))
        d <-
            d[c('latitude',
                'longitude',
                'city',
                'country',
                'casualties')]
        dist_city <- distinct(d, d$city, .keep_all = TRUE)
        d2 <- d %>% count(city) %>% group_by(city)
        d2 <- d2 %>% arrange(desc(n))
        d2 <- d2[-grep("Unknown", d2$city), ]
        d2 <- left_join(d2, dist_city, by = "city")
        d2 <- d2[!is.na(d2$latitude) & !is.na(d2$longitude), ]
        d2 <-
            d2 %>% select(city, country, n, latitude, longitude) %>% rename(attackcount = n)
        d <-  d2[1:1000, ]
        
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
    
    output$globe <- renderGlobe({
        d <- plot_gtdsub()
        d <-
            d %>% mutate(casualties = ifelse(
                is.na(d$nkill),
                ifelse(is.na(d$nwound), 0, d$nwound),
                ifelse(is.na(d$nwound), d$nkill, d$nkill + d$nwound)
            ))
        d <- d[!is.na(d$latitude) & !is.na(d$longitude),]
        d <- d[!is.na(d$casualties),]
        d <-
            d %>% mutate(size = (d$casualties / max(d$casualties)) * 100)
        d <- sample_n(d, 1000, replace = TRUE)
        
        output$globeText <- renderText({
            paste(
                "The attacks of the depicted dataset contain attacks in ",
                length(unique(d$country)),
                "countries",
                "with a total number of",
                sum(d$nkill, na.rm = TRUE),
                "people killed and",
                sum(d$nwound, na.rm = TRUE),
                "people wounded",
                " (sum of casualties: ",
                sum(d$casualties),
                ")."
            )
        })
        d[, c("latitude", "longitude", "size")]
    })
    
    
    output$missingdata_lite <- renderPlot(#height = 700,
        # width = "auto",
        {
            plot_missing(data_lite)
        })
    
    output$missingdata <- renderPlot(#height = 700,
        #width = "auto",
        {
            plot_missing(data)
        })
    
    
    output$killings1 <- renderPlot({
        d <- plot_gtdsub()
        d %>% filter(nkill > 0) -> dfk
        treemap(
            dfk,
            index = c("iyear"),
            vSize = "nkill",
            palette = "Reds",
            fontsize.title = 14
        )
    })
    
    output$killings2 <- renderPlot({
        d <- plot_gtdsub()
        d %>% filter(nkill > 0) -> dfk
        treemap(
            dfk,
            index = c("country"),
            vSize = "nkill",
            palette = "Reds",
            fontsize.title = 14
        )
    })
    
    output$killings3 <- renderPlot({
        d <- plot_gtdsub()
        d %>% filter(nkill > 0) -> dfk
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
        d <- plot_gtdsub()
        valueBox(
            subtitle = "Total Rows",
            value = nrow(d),
            icon = icon("stream")
        )
    })
    
    output$total_years <- renderValueBox({
        d <- plot_gtdsub()
        df_uniq <- unique(d$iyear)
        valueBox(
            subtitle = "Total Years",
            value = length(df_uniq),
            icon = icon("stream"),
            color = "light-blue",
        )
    })
    
    output$total_countries <- renderValueBox({
        d <- plot_gtdsub()
        df_uniq <- unique(d$country)
        valueBox(
            subtitle = "Total Countries",
            value = length(df_uniq),
            icon = icon("stream"),
            color = "light-blue"
        )
    })
    output$total_cities <- renderValueBox({
        d <- plot_gtdsub()
        df_uniq <- unique(d$city)
        valueBox(
            subtitle = "Total Cities",
            value = length(df_uniq),
            icon = icon("stream"),
            color = "light-blue"
        )
    })
    
    output$total_killed <- renderValueBox({
        d <- plot_gtdsub()
        d %>% filter(nkill > 0) -> dfk
        dfk %>% summarise(nkills = sum(nkill)) -> dfyr
        valueBox(
            subtitle = "Total Killed",
            value = dfyr,
            icon = icon("stream"),
            color = "red"
        )
    })
    
    output$total_wounded <- renderValueBox({
        d <- plot_gtdsub()
        d %>% filter(nwound > 0) %>% summarise(nkills = sum(nwound)) -> dfyr
        valueBox(
            subtitle = "Total Wounded",
            value = dfyr,
            icon = icon("stream"),
            color = "orange"
        )
    })
    
    output$word_cloud1 <- renderPlot(height = 500,
                                     width = "auto",
                                     {
                                         data %>% filter(!is.na(summary)) -> dfn0
                                         dfn0 %>% filter(summary != "") -> dfn
                                         text <-
                                             sample(dfn$summary, nrow(dfn) / 100)
                                         myCorpus <-
                                             Corpus(VectorSource(text))
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
                                         freqTerms <-
                                             findFreqTerms(myDtm, lowfreq = 1)
                                         m <- as.matrix(myDtm)
                                         # calculate the frequency of words
                                         v <-
                                             sort(rowSums(m), decreasing = TRUE)
                                         myNames <- names(v)
                                         d <-
                                             data.frame(word = myNames, freq = v)
                                         wctop <-
                                             wordcloud(d$word,
                                                       d$freq,
                                                       min.freq = 50,
                                                       colors = brewer.pal(9, "Set1"))
                                     })
    
    output$dendogram1 <- renderPlot(height = 500,
                                    width = "auto",
                                    {
                                        data %>% filter(!is.na(summary)) -> dfn0
                                        dfn0 %>% filter(summary != "") -> dfn
                                        text <-
                                            sample(dfn$summary, nrow(dfn) / 100)
                                        myCorpus <-
                                            Corpus(VectorSource(text))
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
                                        mydata.df <-
                                            as.data.frame(inspect(removeSparseTerms(myDtm, sparse = 0.99)))
                                        mydata.df.scale <-
                                            scale(mydata.df)
                                        d <-
                                            dist(mydata.df.scale, method = "euclidean") # distance matrix
                                        fit <-
                                            hclust(d, method = "ward.D")
                                        plot(
                                            fit,
                                            xaxt = 'n',
                                            yaxt = 'n',
                                            xlab = "Word clustering using ward.D method",
                                            ylab = "",
                                            main = "Cluster Dendogram for words used in summary description"
                                        ) # display dendogram?
                                        groups <-
                                            cutree(fit, k = 5) # cut tree into 5 clusters
                                        # draw dendogram with blue borders around the 5 clusters
                                        rect.hclust(fit, k = 5, border = "blue")
                                    })
    
    output$downloadData_clean <- downloadHandler(
        filename = "data_clean.csv",
        content = function(file) {
            write.csv(data_lite, file)
        }
    )
    
    output$downloadData <- downloadHandler(
        filename = "data.csv",
        content = function(file) {
            write.csv(data, file)
        }
    )
    
    output$correlation_1 <- renderPlot({
        d <- plot_gtdsub()
        plot_correlation(d,
                         type = 'continuous',
                         cor_args = list("use" = "pairwise.complete.obs"))
    })
    
})