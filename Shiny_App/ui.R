library(shiny)
library(shinydashboard)
library(DT)


header <- dashboardHeader(
    title = "Global Terrorism Database"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Data Overview", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Multivariate Analysis", icon = icon("chart-bar"), tabName = "multivar")
    )
)


body <- dashboardBody(
    
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Data Overview"),
                DT::dataTableOutput("datatable")
        ),
        
        tabItem(tabName = "multivar",
                h2("Overview of Multivariate Analysis")
        )
    )

)

dashboardPage(
    header,
    sidebar,
    body
)
