library(shiny)
library(shinydashboard)
library(DT)
library(png)
library(DataExplorer)
library(treemap)
require(dplyr)
library(tm)
library(wordcloud)


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
        text <- sample(dfn$summary, nrow(dfn)/100)
        myCorpus <- Corpus(VectorSource(text))
        #myCorpus = tm_map(myCorpus, content_transformer(tolower))
        # remove punctuation
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        # remove stopwords for English
        myCorpus = tm_map(myCorpus, removeWords,c(stopwords("english"), stopwords("SMART"), "the"))
        #create DTM
        myDtm = TermDocumentMatrix(myCorpus,
                                   control = list(minWordLength = 3))
        #Frequent Terms and Associations
        freqTerms <- findFreqTerms(myDtm, lowfreq=1)
        m <- as.matrix(myDtm)
        # calculate the frequency of words
        v <- sort(rowSums(m), decreasing=TRUE)
        myNames <- names(v)
        d <- data.frame(word=myNames, freq=v)
        wctop <-wordcloud(d$word, d$freq, min.freq=50, colors=brewer.pal(9,"Set1"))
    })
    
    output$dendogram1 <- renderPlot({
        mydata.df <- as.data.frame(inspect(removeSparseTerms(myDtm, sparse=0.99)))
        mydata.df.scale <- scale(mydata.df)
        d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward.D")
        plot(fit, xaxt = 'n', yaxt='n', xlab = "Word clustering using ward.D method", ylab = "",
             main="Cluster Dendogram for words used in summary description") # display dendogram?
        groups <- cutree(fit, k=5) # cut tree into 5 clusters
        # draw dendogram with blue borders around the 5 clusters
        rect.hclust(fit, k=5, border="blue")
    })
    
})
