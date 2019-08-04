library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinycssloaders)



header <- dashboardHeader(
    title = "Global Terrorism Database"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        id="tabitems",
        menuItem("Data Overview", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Univariate Analysis", tabName = "univar", icon = icon("chart-bar")),
        menuItem("Multivariate Analysis", tabName = "multivar", icon = icon("chart-bar")),
        menuItem("Map", tabName = "map", icon = icon("map")),

        withSpinner(uiOutput("yearSelection")),
        uiOutput("regionSelection"),
        uiOutput("countrySelection"),
        uiOutput("attackSelection")
    )
)


body <- dashboardBody(
    
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Data Overview"),
                withSpinner(DT::dataTableOutput("datatable"))
        ),
        
        tabItem(tabName = "univar",
                
                fluidRow(
                    h2("Univariate Analysis"),
                    
                    valueBoxOutput("attack_year"),
                    valueBoxOutput("casual_att")
                ),
                
                fluidRow(tabBox(
                    
                    title = "Attack Count Distributions",
                    
                    tabPanel(
                        "By Year",
                        h3(""),
                        withSpinner(plotlyOutput("distyear"))
                        #includeMarkdown("killings1.md")
                    ),
                    
                    tabPanel(
                        "By Region",
                        h3(""),
                        withSpinner(plotlyOutput("dist_region1"))
                        #includeMarkdown("killings2.md")
                        
                    ),
                    
                    tabPanel(
                        "By Attack Type",
                        h3(""),
                        withSpinner(plotlyOutput("dist_attack"))
                        #includeMarkdown("killings3.md")
                    ),
                    
                    tabPanel(
                        "By Weapon Type",
                        h3(""),
                        withSpinner(plotlyOutput("dist_weap"))
                        #includeMarkdown("killings3.md")
                    )
                ))
                
                # fluidRow(
                #     h2("Overview of Univariate Analysis"),
                #     column(width=6, 
                #            box(width = NULL, solidHeader = TRUE, plotlyOutput("distyear"))
                #     ),
                #     
                #     column(width=6, 
                #            box(width = NULL, solidHeader = TRUE, plotlyOutput("dist_region1"))
                #     )
                #     
                # ),
                # fluidRow(
                #     column(width=6, 
                #            box(width = NULL, solidHeader = TRUE, plotlyOutput("dist_country"))
                #     ),
                #     
                #     column(width=6, 
                #            box(width = NULL, solidHeader = TRUE, plotlyOutput("dist_attack"))
                #     )
                # )
                
        ),
        
        # tabItem(tabName = "univar2",
        #         h2("Overview of Univariate Analysis 2"),
        #         plotlyOutput("dist_region2"),
        #         plotlyOutput("distyear2")
        # ),
        
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
