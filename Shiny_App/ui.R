library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(markdown)

header <- dashboardHeader(title = "Global Terrorism Database",
                          titleWidth = 300)

sidebar <- dashboardSidebar(
    width = 300,
    sidebarMenu(
        menuItem(
            "Overview/Contents",
            icon = icon("list-ul"),
            tabName = "page1"
        ),
        menuItem(
            "Data Preparation",
            icon = icon("database"),
            tabName = "page2"
        ),
        menuItem("Data Explorer", icon = icon("table"), tabName = "page3"),
        menuItem(
            "Univariate Analysis",
            icon = icon("chart-pie"),
            tabName = "page4"
        ),
        menuItem(
            "Multivariate Analysis",
            icon = icon("columns"),
            tabName = "page5"
        ),
        menuItem(
            "Geospatial Analysis",
            icon = icon("map"),
            tabName = "page6"
        )
    )
)


body <- dashboardBody(tabItems(
    tabItem(tabName = "page1",
            h2("Overview & Table of Contents")),
    
    tabItem(tabName = "page2",
            fluidRow(h2(
                "Preparation of Raw Data"
            )),
            fluidRow(
                tabBox(
                    title = "Missing Values",
                    tabPanel(
                        "Original Dataset",
                        h3("Total of 135 variables with a lot of missing values"),
                        plotOutput("raw_missing", width = "100%", height = "250px")
                    ),
                    tabPanel(
                        "Cleand Dataset",
                        h3(
                            "Total of 33 variables with cleaned data and just a few missing values"
                        ),
                        plotOutput("lite_missing", width = "100%", height = "250px")
                    )
                )
            )),
    tabItem(
        tabName = "page3",
        fluidRow(
            h2("Exploring the Dataset"),
            valueBoxOutput("total_values"),
            valueBoxOutput("total_years"),
            valueBoxOutput("total_countries"),
            valueBoxOutput("total_cities"),
            valueBoxOutput("total_wounded"),
            valueBoxOutput("total_killed")
            
        ),
        DT::dataTableOutput("data_explorer")
    ),
    tabItem(tabName = "page4",
            h2("Univariate Insights into Global Terror")),
    tabItem(
        tabName = "page5",
        h2("Multivariate Insights into Global Terror"),
        fluidRow(tabBox(
            title = "Killings",
            tabPanel(
                "Year",
                h3("Overall killings per Year"),
                withSpinner(plotOutput('killings1')),
                includeMarkdown("killings1.md")
                
            ),
            tabPanel(
                "Country",
                h3("Overall killings per Country"),
                withSpinner(plotOutput('killings2')),
                includeMarkdown("killings2.md")
            ),
            tabPanel(
                "Countries and Years",
                h3(
                    "Killings per Countries and Years - size is proportional with the number of killings"
                ),
                withSpinner(plotOutput('killings3')),
                includeMarkdown("killings3.md")
            )
        ))
    ),
    tabItem(tabName = "page6",
            h2("Geospatial Insights into Global Terror"))
))

dashboardPage(skin = "black",
              header,
              sidebar,
              body)
