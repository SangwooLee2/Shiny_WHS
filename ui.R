#unesco

shinyUI(dashboardPage(
  dashboardHeader(title = "UNESCO World Heritage Sites"),
  dashboardSidebar(
    
    sidebarUserPanel("Sangwoo Lee",
                     image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("book")),
      menuItem("Statistics", tabName = "statistics", icon = icon("line-chart")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Data by country", tabName = "pernation", icon = icon("database")),
      menuItem("Locations on world map", tabName = "world", icon = icon("map"))      
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "about",
              fluidPage(
                h1("Unesco World Heritage Sites Shiny App", align="center"),
                br(), 
                br(), 
                h3("This is a R-Shiny project created by using the dataset(*) provided by Unesco(**).",align="left"),
                h3("Using the dataset and the R packages, data and maps of current status of world historic sites are provided.", align="left"),
                br(),
                br(),
                img(src="mosaic001.jpg", width="50%", height="50%"),
                br(),
                br(),
                h4("(*): https://whc.unesco.org/en/syndication, Copyright © 1992 - 2019 UNESCO/World Heritage Centre. All rights reserved.", align="left"),
                h4("(**): https://whc.unesco.org", align="left")
              )),  
    
      tabItem(tabName = "statistics",
              fluidPage(
                h1("Statistics", align="center"),
                br(), 
                br(), 
                fluidRow(column(width = 6,plotlyOutput("No_per_state", height = 500)),
                         column(width = 6, plotlyOutput("No_per_region", height = 500)))
                # ,
                # fluidRow(DT::dataTableOutput("Noperstate_table"))
                
              )),      
        
      tabItem(tabName = "data",
              fluidPage(
                h1("Data of world heritage sites", align="center"),
                fluidRow(DT::dataTableOutput("whsites"))
              )),
      
      tabItem(tabName = "pernation",
              fluidPage(
                h1("Data of world heritage sites by country", align="center"),
                br(),

                
                fluidRow(column(width = 4, selectizeInput(inputId = 'stateID', label = "country", choices = state_vec)),
                         column(width = 4, uiOutput("dynamic_widget"))),       
              
                fluidRow(DT::dataTableOutput("whsites_state_cat")),
                
                fluidRow(column(width = 6,plotlyOutput("BarplotPerYearPerNation", height = 500)))
              )),      
      
      tabItem(tabName = "world",
              h2('Unesco world heritage sites on world map'),
              fluidRow
              (
                # column(width = 9, plotlyOutput("world_map", height = 700), solidHeader = TRUE, status = "primary")
                column(width = 12, plotlyOutput("world_map", height = 900), solidHeader = TRUE, status = "primary")
              )
      )
      
      
      
    )
  )
))