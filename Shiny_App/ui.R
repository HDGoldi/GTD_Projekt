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
                    width = 12,
                    height = "auto",
                    h2("Global-Terrorism Database"),
                    style = "font-size: 120%; background-color : #29abe2;",
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
                    width = 12,
                    height = "auto",
                    h2("Data Source"),
                    style = "font-size: 120%; background-color : #77D245;",
                    solidHeader = TRUE,
                    p(
                        "GTD can be obtained from",
                        a(
                            strong("Kaggle"),
                            href = "https://www.kaggle.com/START-UMD/gtd",
                            target = "_blank",
                            style = "color :white; font-weight : bold;"
                        ),
                        "The clean data set used in this application is also available to download.",
                        "For everyone interested in working with the same dataset we provide the RAW data we used plus the prepared and cleaned data for download:"
                    ),
                    downloadButton('downloadData_clean', 'Download Clean Data'),
                    downloadButton('downloadData', 'Download RAW Data')
                ),
                box(
                    width = 12,
                    height = "auto",
                    h2("Big data cleaning required for RAW Data"),
                    style = "font-size: 120%; background-color : #FF9F00;",
                    solidHeader = TRUE,
                    p(
                        "GTD contains 135 variables with a lot of missing values in the RAW version",
                        "Therefore we started of by kicking out missing values based on a quick analysis of the different features."
                    ),
                    withSpinner(plotOutput('missingdata')),
                    p(
                        "Kicking out many of the features with a high amount of missing values we condenst the dataset to 33 variables.",
                        "These features have just a few missing values, mostly for the summary text of the event."
                    ),
                    withSpinner(plotOutput('missingdata_lite'))
                )
            )),
    
    tabItem(tabName = "page3",
            h2("Exploring the Dataset"),
            fluidRow(
                width = 12,
                valueBoxOutput("total_values"),
                valueBoxOutput("total_years"),
                valueBoxOutput("total_countries"),
                valueBoxOutput("total_cities"),
                valueBoxOutput("total_wounded"),
                valueBoxOutput("total_killed")
            ),
            withSpinner(DT::dataTableOutput("datatable"))),

    tabItem(
        tabName = "page4",
        h2("Univariate Insights into Global Terror"),
        fluidRow(box(width = 12,
                     p(
                            "One of the major challenges with the univariate analysis of this dataset was the low quantity of metric variables. 
                             Out of the 32 variables in the cleansed dataset only 4 of them were scaled metrical and contained enough data to be used 
                             for a closer look at measures of location and dispersion. 
                             But first we will take a look at the frequency distributions of some of the more interesting nominal variables:"
                     ))),

        fluidRow(
            tabBox(
                title = "Frequency Distributions (Reactive)",
                width = 12,
                tabPanel("By Year", h3(""), withSpinner(plotlyOutput("atdist_year"))),
                tabPanel("By Region", h3(""), withSpinner(plotlyOutput("atdist_region"))),
                tabPanel("By Attack Type", h3(""), withSpinner(plotlyOutput("atdist_attack"))),
                tabPanel("By Weapon Type", h3(""), withSpinner(plotlyOutput("atdist_weap")))
            )),
            
        fluidRow(width = 12,
                     valueBoxOutput("attack_year"),
                     valueBoxOutput("casual_att")
            ),

        fluidRow(box(width = 12,
                         p(
                             "The 4 major metric variables are the number of killed and wounded people and terrorist for each incident. 
                              When we look at the boxplots for these values, we see that the range of the values is quite high with some
                              extreme outliers at the top, while the median is close to 0."
                         ))),                        
            
        fluidRow(box(
            width = 12,
            column(width  = 3,
                   withSpinner(plotOutput("boxplot1"))),
            column(width  = 3,
                   withSpinner(plotOutput("boxplot2"))),
            column(width  = 3,
                   withSpinner(plotOutput("boxplot3"))),
            column(width  = 3,
                   withSpinner(plotOutput("boxplot4")))
        )),
        
        fluidRow(box(width = 12,
                     p(
                         "Once those outliers are removed we can see that for the killed (nkill) and wounded (nwound) 75% of the values 
                         fall between 0 and 2. Which makes the Interquartile range (IQR) of those variables 2.  While for the killed and 
                         wounded terrorists (nkillter and nwoundte) we see that with the ouliers removed, 100% of the values are 0. 
                         Because of that we will be focusing on the number of casualties (sum of the killed and wounded without terrorists) 
                         in the further examinations of this dataset."
                     ))),  
        
        fluidRow(box(
            width = 12,
            column(width  = 3,
                   withSpinner(plotOutput("boxplot5"))),
            column(width  = 3,
                   withSpinner(plotOutput("boxplot6"))),
            column(width  = 3,
                   withSpinner(plotOutput("boxplot7"))),
            column(width  = 3,
                   withSpinner(plotOutput("boxplot8")))
        )),
        
        fluidRow(box(width = 12,
                     p(
                         "As the boxplots above did already show, the observed variables are not distributed normally.
                         This hyptohesis can be backed by running the Shapiro-Wilk Normality Test (shapiro.test) on a sample of 5000 rows.
                         This function returns a p-value of 2.2∗10^-16 which far less than 0.05 implying that the distribution of the 
                         data is significantly different from normal distribution. "
                     )))
    ),
    
    tabItem(tabName = "page5",
            h2("Multivariate Insights into Global Terror"),
            fluidRow(
                tabBox(
                    title = "Casualty Count Distributions by different Dimensions",
                    width = 12,
                    tabPanel("By Year",h3(""), withSpinner(plotlyOutput("casdist_year"))),
                    tabPanel("By Region",h3(""), withSpinner(plotlyOutput("casdist_region"))),
                    tabPanel("By Attack Type",h3(""), withSpinner(plotlyOutput("casdist_attack"))),
                    tabPanel("By Weapon Type",h3(""), withSpinner(plotlyOutput("casdist_weap")))
                )),
            fluidRow(
                tabBox(
                    title = "Detailed view on Attacks by Region and Year together with Attacktype used",
                    width = 12,
                    tabPanel("Attacks By Attacktype & Region", h3(""), withSpinner(plotlyOutput("dist_region2"))),
                    tabPanel("Attacks By Region & Year", h3(""), withSpinner(plotlyOutput("distyear2")))
                ) 
            ),
            fluidRow(
                tabBox(
                    title = "Killings",
                    width = 12,
                    tabPanel("Year",h3("Overall killings per Year"), withSpinner(plotOutput('killings1')), includeMarkdown("killings1.md")),
                    tabPanel("Country",h3("Overall killings per Country"), withSpinner(plotOutput('killings2')), includeMarkdown("killings2.md")),
                    tabPanel("Countries & Years",h3("Killings per Countries and Years - size is proportional with the number of killings"), withSpinner(plotOutput('killings3')), includeMarkdown("killings3.md"))
                )
            ),
            fluidRow(
                tabBox(
                    width = 12,
                    title = "Summary - Text Insights",
                    tabPanel("Wordcloud for Summary Text", withSpinner(plotOutput('word_cloud1'))),
                    tabPanel("Denogram for Summary Text", withSpinner(plotOutput('dendogram1')))
                )
            )),
    
    tabItem(tabName = "page6",
            fluidRow(box(
                width = 12,
                h2("Top 1000 Cities by Attack Count"),
                withSpinner(leafletOutput("map"))
            )),
            fluidRow(
                box(
                    width = 12,
                    height = 1000,
                    title = "Globe View",
                    textOutput("globeText"),
                    withSpinner(globeOutput("globe"))
                )
            ))
))
    

dashboardPage(skin = "black",
              header,
              sidebar,
              body)
