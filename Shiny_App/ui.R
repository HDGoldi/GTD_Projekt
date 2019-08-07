library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(markdown)
library(plotly)
library(leaflet)
library(shinyGlobe)

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
            h2("Overview & Table of Contents"),
            fluidRow(
                box(
                    h2("Global-Terrorism Database"),
                    background = "light-blue",
                    solidHeader = TRUE,
                    p(
                        "The Global Terrorism Database (GTD) is an open-source database including information on
                                         terrorist attacks around the world from 1970 through 2015 (with annual updates planned for
                                         the future). The GTD includes systematic data on domestic as well as international terrorist
                                         incidents that have occurred during this time period, with more than 150,000
                                         cases. The database is maintained by researchers at the National Consortium for the Study
                                         of Terrorism and Responses to Terrorism (START), headquartered at the University of
                                         Maryland."
                    ),
                    tags$ul(
                        tags$li("Time period: 1970-2017, except 1993"),
                        tags$li("Unit of analysis: Attack"),
                        tags$li("Variables: >100 variables on location, tactics, perpetrators, targets, and outcomes"),
                        tags$li("Sources: Unclassified media articles.")
                    )
                    
                ),
                box(
                    h2("Data Source"),
                    style = "font-size: 120%; background-color : #e77e7e;",
                    solidHeader = TRUE,
                    p(
                        "GTD can be obtained from",
                        a(
                            strong("Kaggle"),
                            href = "https://www.kaggle.com/START-UMD/gtd",
                            target = "_blank",
                            style = "color :white; font-weight : bold;"
                        ),
                        "The clean data set used in this application is also available to download."
                    ),
                    downloadButton('downloadData_clean', 'Download Clean Data'),
                    downloadButton('downloadData', 'Download RAW Data')
                )
            )),
    tabItem(tabName = "page2",
            h2("Preparation of Raw Data"),
            fluidRow(
                tabBox(
                    title = "Missing Values",
                    height = "200",
                    tabPanel("Original Dataset", h3("Total of 135 variables with a lot of missing values"), withSpinner(plotOutput('missingdata'))),
                    tabPanel("Cleaned Dataset", h3("Total of 33 variables with cleaned data and just a few missing values"), withSpinner(plotOutput('missingdata_lite')))
                )
            )),
    tabItem(tabName = "page3",
            h2("Exploring the Dataset"),
            fluidRow(
                valueBoxOutput("total_values"),
                valueBoxOutput("total_years"),
                valueBoxOutput("total_countries"),
                valueBoxOutput("total_cities"),
                valueBoxOutput("total_wounded"),
                valueBoxOutput("total_killed")
            ),
            withSpinner(DT::dataTableOutput("datatable"))),

    tabItem(tabName = "page4",
            h2("Univariate Insights into Global Terror"),
            fluidRow(box(width = 12,
                         includeMarkdown("univariat.md")) 
            ),
            fluidRow(
                box(width = 12, 
                    column(width  = 3,
                           withSpinner(plotOutput("boxplot1"))
                    ),
                    column(width  = 3,
                           withSpinner(plotOutput("boxplot2"))
                    ),
                    column(width  = 3,
                           withSpinner(plotOutput("boxplot3"))
                    ),
                    column(width  = 3,
                           withSpinner(plotOutput("boxplot4"))
                    )
                )
            ),
            
            fluidRow(box(width = 12,
                         includeMarkdown("univariat2.md")) 
            ),
            fluidRow(
                box(width = 12,
                    column(width  = 3,
                           withSpinner(plotOutput("boxplot5"))
                    ),
                    column(width  = 3,
                           withSpinner(plotOutput("boxplot6"))
                    ),
                    column(width  = 3,
                           withSpinner(plotOutput("boxplot7"))
                    ),
                    column(width  = 3,
                           withSpinner(plotOutput("boxplot8"))
                    )
                )
            ),
            
            fluidRow(box(width = 12,
                         includeMarkdown("univariat3.md")) 
            ),
            
            fluidRow(
                tabBox(
                    title = "Casualty Count Distributions (Multivariat?)",
                    width = "12",
                    tabPanel("By Year",h3(""), withSpinner(plotlyOutput("casdist_year"))),
                    tabPanel("By Region",h3(""), withSpinner(plotlyOutput("casdist_region"))),
                    tabPanel("By Attack Type",h3(""), withSpinner(plotlyOutput("casdist_attack"))),
                    tabPanel("By Weapon Type",h3(""), withSpinner(plotlyOutput("casdist_weap")))
                )
                
            ),
            
            
            fluidRow(valueBoxOutput("attack_year"),
                     valueBoxOutput("casual_att")
                     
            ),
            fluidRow(
                tabBox(
                    title = "Attack Count Distributions",
                    width = "12",
                    tabPanel("By Year",h3(""), withSpinner(plotlyOutput("atdist_year"))),
                    tabPanel("By Region",h3(""), withSpinner(plotlyOutput("atdist_region"))),
                    tabPanel("By Attack Type",h3(""), withSpinner(plotlyOutput("atdist_attack"))),
                    tabPanel("By Weapon Type",h3(""), withSpinner(plotlyOutput("atdist_weap")))
                )
                
            ),
            fluidRow(
                tabBox(
                    title = "Multivariate?",
                    width = "12",
                    tabPanel("Attacks By Attacktype & Region", h3(""), withSpinner(plotlyOutput("dist_region2"))),
                    tabPanel("Attacks By Region & Year", h3(""), withSpinner(plotlyOutput("distyear2")))
                )
                
            )),
    
    tabItem(tabName = "page5",
            h2("Multivariate Insights into Global Terror"),
            fluidRow(
                tabBox(
                    title = "Killings",
                    width = "12",
                    tabPanel("Year",h3("Overall killings per Year"), withSpinner(plotOutput('killings1')), includeMarkdown("killings1.md")),
                    tabPanel("Country",h3("Overall killings per Country"), withSpinner(plotOutput('killings2')), includeMarkdown("killings2.md")),
                    tabPanel("Countries & Years",h3("Killings per Countries and Years - size is proportional with the number of killings"), withSpinner(plotOutput('killings3')), includeMarkdown("killings3.md"))
                )
            ),
            fluidRow(
                tabBox(
                    title = "Summary - Text Insights",
                    tabPanel("Wordcloud for Summary Text", withSpinner(plotOutput('word_cloud1'))),
                    tabPanel("Denogram for Summary Text", withSpinner(plotOutput('dendogram1')))
                )
            )),
    tabItem(tabName = "page6",
            fluidRow(box(
                width = "12",
                h2("Top 1000 Cities by Attack Count"),
                withSpinner(leafletOutput("map"))
            ),
            fluidRow(
                box(
                    width = "12",
                    height = 1000,
                    title = "Globe View",
                    textOutput("globeText"),
                    withSpinner(globeOutput("globe"))
                )
            )))
))

dashboardPage(skin = "black",
              header,
              sidebar,
              body)
