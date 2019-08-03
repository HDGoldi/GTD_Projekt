library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)


header <- dashboardHeader(
    title = "Global Terrorism"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview/Contents", icon = icon("list-ul"), tabName = "page1"),
        menuItem("Data Preparation", icon = icon("database"), tabName = "page2"),
        menuItem("Data Explorer", icon = icon("table"), tabName = "page3"),
        menuItem("Univariate Analysis", icon = icon("chart-pie"), tabName = "page4"),
        menuItem("Multivariate Analysis", icon = icon("columns"), tabName = "page5"),
        menuItem("Geospatial Analysis", icon = icon("map"), tabName = "page6")
    )
)


body <- dashboardBody(
    
    tabItems(
        tabItem(tabName = "page1",
                h2("Overview & Table of Contents")
        ),
        
        tabItem(tabName = "page2",
                h2("Preparation of Raw Data"),
                fluidRow(
                  tabBox(
                      title = "Missing Values",
                      tabPanel("Original Dataset",
                               h3("Total of 135 variables with a lot of missing values"),
                               plotOutput("raw_missing", width = "100%", height = "250px")      
                      ),
                      tabPanel("Cleand Dataset",
                               h3("Total of 33 variables with cleaned data and just a few missing values"),
                               plotOutput("lite_missing", width = "100%", height = "250px")
                               )
                  )  
                )

        ),
        tabItem(tabName = "page3",
                h2("Exploring the Dataset"),
                DT::dataTableOutput("data_explorer")
        ),
        tabItem(tabName = "page4",
                h2("Univariate Insights into Global Terror")
        ),
        tabItem(tabName = "page5",
                h2("Multivariate Insights into Global Terror"),
                fluidRow(
                    tabBox(
                        title = "Killings",
                        tabPanel("Year",
                                 h3("Overall killings in Global Terrorism"),
                                 plotOutput('killings1')%>% withSpinner(color="#0dc5c1")
                        ),
                        tabPanel("Country",
                                 h3("Overall killings in Global Terrorism"),
                                 plotOutput('killings2')%>% withSpinner(color="#0dc5c1")
                        ),
                        tabPanel("Countries and Years",
                                 h3("Killings in Global terrorism  (Countries/Years) - size is proportional with the number of killings"),
                                 plotOutput('killings3')%>% withSpinner(color="#0dc5c1")
                        )
                    )  
                )
        ),
        tabItem(tabName = "page6",
                h2("Geospatial Insights into Global Terror")
        )
    )

)

dashboardPage(
    header,
    sidebar,
    body
)
