library(shiny)
library(shinydashboard)
library(DT)
library(plotly)



header <- dashboardHeader(
    title = "Global Terrorism Database"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Data Overview", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Univariate Analysis 1", tabName = "univar1", icon = icon("chart-bar")),
        menuItem("Univariate Analysis 2", tabName = "univar2", icon = icon("chart-bar")),
        menuItem("Multivariate Analysis", tabName = "multivar", icon = icon("chart-bar")),
        menuItem("Map", tabName = "map", icon = icon("map")),

        sliderInput("year",
                    "Please select the period ",
                    min = 1970,
                    max = 2017,
                    value = c(1970,2017),
                    sep = ""
        ),
        uiOutput("regionSelection"),
        uiOutput("countrySelection"),
        uiOutput("attackSelection")
    )
)


body <- dashboardBody(
    
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Data Overview"),
                DT::dataTableOutput("datatable")
        ),
        
        tabItem(tabName = "univar1",
                fluidRow(
                    h2("Overview of Univariate Analysis"),
                    column(width=6, 
                           box(width = NULL, solidHeader = TRUE, plotlyOutput("distyear"))
                    ),
                    
                    column(width=6, 
                           box(width = NULL, solidHeader = TRUE, plotlyOutput("dist_region1"))
                    )
                    
                ),
                fluidRow(
                    column(width=6, 
                           box(width = NULL, solidHeader = TRUE, plotlyOutput("dist_country"))
                    ),
                    
                    column(width=6, 
                           box(width = NULL, solidHeader = TRUE, plotlyOutput("dist_attack"))
                    )
                )
                
                # fluidRow(
                #     column(width=6, 
                #            box(width = NULL, solidHeader = TRUE, plotlyOutput("dist_weap"))
                #     ),
                #     
                #     column(width=6, 
                #            box(width = NULL, solidHeader = TRUE )
                #     )
                # )
        ),
        
        tabItem(tabName = "univar2",
                h2("Overview of Univariate Analysis 2"),
                plotlyOutput("dist_region2"),
                plotlyOutput("distyear2")
        ),
        
        tabItem(tabName = "multivar",
                h2("Overview of Multivariate Analysis")
        ),
        
        tabItem(tabName = "map",
                h2("Top 100 Cities by Attack Count"),
                leafletOutput("map")
        )
    )
)

dashboardPage(
    header,
    sidebar,
    body
)
