library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(markdown)
library(plotly)
library(leaflet)

header <- dashboardHeader(title = "Global Terrorism Database",
                          titleWidth = 300)


sidebar <- dashboardSidebar(
    width = 300,
    sidebarMenu(
        menuItem(
            "Overview/Contents",
            icon = icon("dashboard"),
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
            icon = icon("chart-bar"),
            tabName = "page5"
        ),
        menuItem(
            "Geospatial Analysis",
            icon = icon("map"),
            tabName = "page6"
        ),
        
        withSpinner(uiOutput("yearSelection")),
        uiOutput("regionSelection"),
        uiOutput("countrySelection"),
        uiOutput("attackSelection")
    )
)


body <- dashboardBody(tabItems(
    tabItem(tabName = "page1",
            h2("Overview & Table of Contents")),
    tabItem(tabName = "page2",
            h2("Preparation of Raw Data"),
            fluidRow(
                tabBox(
                    title = "Missing Values",
                    tabPanel(
                        "Original Dataset",
                        h3("Total of 135 variables with a lot of missing values"),
                        plotOutput("raw_missing")
                    ),
                    tabPanel(
                        "Cleand Dataset",
                        h3(
                            "Total of 33 variables with cleaned data and just a few missing values"
                        ),
                        plotOutput("lite_missing")
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
        withSpinner(DT::dataTableOutput("datatable")),
    ),
    tabItem(
        tabName = "page4",
        h2("Univariate Insights into Global Terror"),
        fluidRow(
            h2("Univariate Analysis"),
            valueBoxOutput("attack_year"),
            valueBoxOutput("casual_att")
        ),
        fluidRow(
            tabBox(
                title = "Attack Count Distributions",
                width = "8",
                tabPanel("By Year",
                         h3(""),
                         withSpinner(plotlyOutput("distyear"))),
                tabPanel("By Region",
                         h3(""),
                         withSpinner(plotlyOutput("dist_region1"))),
                tabPanel("By Attack Type",
                         h3(""),
                         withSpinner(plotlyOutput("dist_attack"))),
                tabPanel("By Weapon Type",
                         h3(""),
                         withSpinner(plotlyOutput("dist_weap")))
            )
        ),
        fluidRow(tabBox(
            title = "Other Title",
            width = "8",
            tabPanel("By Year",
                     h3(""),
                     withSpinner(plotlyOutput("dist_region2"))),
            tabPanel("By Region",
                     h3(""),
                     withSpinner(plotlyOutput("distyear2")))
        )),
    ),
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
        )),
        fluidRow(tabBox(
            title = "Summary",
            tabPanel(
                "Wordcloud for Summary Text",
                h3("Overall killings per Year"),
                withSpinner(plotOutput('word_cloud1'))
            ),
            tabPanel(
                "Denogram for Summary Text",
                h3("Overall killings per Country"),
                withSpinner(plotOutput('dendogram1'))
            )
        ))
    ),
    tabItem(tabName = "page6",
            h2("Top 1000 Cities by Attack Count"),
            fluidRow(box(
                width = "8",
                leafletOutput("map")
            )))
))

dashboardPage(skin = "black",
              header,
              sidebar,
              body)
