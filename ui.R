library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plyr)
library(dplyr)
library(leaflet)
library(rjson)
library(rgdal)



###### Choices for drop-downs ########################################## 
vars_1 <- c("Residential Energy Consumption", 
            "Residential Electricity Consumption", 
            "Heating Degree Day", 
            "Mean Household Income")

vars_2 <- c("Level 1 & 2 - All Variables",
            "Level 1 - Population Density", 
            "Level 1 - Household Income", 
            "Level 1 & 2 - Household Income",
            "Level 1 & 2 - Rural zone and Population Density")

select_var1_LOGHDDDEP <- c("1300 DJU","2000 DJU", "3000 DJU")
select_var2_LOGREVDEP <- c("20 k€", "30 k€", "40 k€")
select_var3_LOGREV <- c("20 k€", "30 k€", "40 k€", "50 k€", "60 k€")
select_var4_AREA3G <- c("0-40m2", "40-100m2", ">100m2")
select_var5_INSULHOUS <- c("Yes", "No")
select_var6_YEARCONST <- c("Before 1975","Between 1975 and 1990","After 1990")
select_var7_ROOMNBR <- c("1","3","5")
select_var8_HEATSYST <- c("Individual","Collective","Mixte")
select_var9_HEATSOURCE <- c("Electricity", "Gas","Other")
select_var10_RURAL <- c("Yes","No")
select_var11_HEATTEMP <- c("Below 21°C", "Above 21°C")
select_var12_ECS <- c("Gas/Fuel/Wood Boiler", "Electricity Boiler", "Other")
select_var13_UNOCCWEEK <- c("< 4 hours per week", "> 4 hours per week")
select_var14_PCS <- c("Executive","Middle-level", "Other")
select_var15_NBRPERS <- c("1","2","3","4","5")


title <- "Phebus Data Analyse"

###### NavBarPage ########################################## 
navbarPage(
           windowTitle="Household Energy Consumption Data Analysis",
           title = div(tags$a(href = 'http://www.cstb.fr', 
                   img(src = 'logov4.png',
                       title = "CSTB", 
                       height = "50px"))),
           id="nav", 
           inverse = FALSE, 
           
           
                                
           # Tab Panel - Multilevel Regression Modelling  #          
           tabPanel("Predictive - Multilevel Regression Modelling", 
                    includeCSS("styles.css"),
                     
                     #### DIV OUTER ####
                     div(class="outer",
                          useShinyjs(),
                         
                          #DIV loading content for loading at server initialisation
                          div(id = "loading-content",
                              img(src="source.gif")),    
                         
                         # Display MRM Model map #    
                         leafletOutput("MRM_Model", 
                                       width="100%", 
                                       height="100%"),
                         # Absolute Panel level 1 predictors #
                         absolutePanel(id = "controls_model", 
                                       style = "overflow-y:scroll; height: 90vh;",
                                       class = "panel panel-default", 
                                       fixed = TRUE,
                                       draggable = FALSE, 
                                       top = 80, 
                                       left = 45, 
                                       width = 250, 
                                       height = "auto",
                                       h2("Select level-1 variables and predict energy consumption"),
                                       box(width = NULL, status = "warning", solidHeader = TRUE,
                                           selectInput("var3_LOGREV",  "Yearly income", select_var3_LOGREV, selected = "30 k€", selectize = FALSE),
                                           selectInput("var4_AREA3G",  "Surface Area of housing", select_var4_AREA3G, selected = "40-100m2", selectize = FALSE),
                                           selectInput("var5_INSULHOUS", "Semi-detached housing", select_var5_INSULHOUS, selected = "No", selectize = FALSE),
                                           selectInput("var6_YEARCONST",  "Housing year of construction", select_var6_YEARCONST,  selected = "Before 1975", selectize = FALSE),
                                           selectInput("var7_ROOMNBR",  "Number of rooms", select_var7_ROOMNBR, selected = "3", selectize = FALSE),
                                           selectInput("var8_HEATSYST",  "Heating system", select_var8_HEATSYST, selected = "Individual", selectize = FALSE),
                                           selectInput("var9_HEATSOURCE", "Space heating energy type", select_var9_HEATSOURCE, selected = "Gas", selectize = FALSE),
                                           selectInput("var10_RURAL", "Rural Municipality", select_var10_RURAL, selected = "No", selectize = FALSE),
                                           selectInput("var11_HEATTEMP", "Heating temperature in housing", select_var11_HEATTEMP, selected = "Below 21°C", selectize = FALSE),
                                           selectInput("var12_ECS", "Water heating system", select_var12_ECS, selected = "Gas/Fuel/Wood Boiler", selectize = FALSE),
                                           selectInput("var13_UNOCCWEEK", "Housing unoccupied", select_var13_UNOCCWEEK, selected = "< 4 hours per week", selectize = FALSE),
                                           selectInput("var14_PCS", "Socio-professional status", select_var14_PCS, selected = "Other", selectize = FALSE),
                                           selectInput("var15_NBRPERS", "Household composition", select_var15_NBRPERS, selected = "3", selectize = FALSE)
                                            )),
                        # Text on he right of AbsolutePanel
                         # tags$div(id="text_milieu",
                         #          
                         #          HTML("Select<br>
                         #               and Predict")),
                        
                        # Tag for abstract button
                        tags$div(id="Button", 
                                 actionButton("goButton", "- About the App -"), 
                                 htmlOutput("text")),
                        
                        # Tag for credits at the bottom
                        tags$div(id="cite",
                                 HTML("Data Source : Phebus Survey (2013)<br>
                                      Credits : Rémy Zumbiehl (CSTB, 2017)"))
                                  
                        )
           ),

           # Tab Panel - Indicators #          
           tabPanel("Descriptive - Map of indicators",
                    includeCSS("styles.css"),
             
                    #### DIV OUTER ####
                    div(class="outer",
                        
                        # Display map of selected indicator # 
                        leafletOutput("map_departement", width="100%", height="100%"),
                        
                        # Absolute Panel indicators
                        absolutePanel(id = "controls", 
                                      class = "panel panel-default", 
                                      fixed = TRUE,
                                      draggable = TRUE, 
                                      top = 80, 
                                      right = 10, 
                                      width = 230, 
                                      height = "auto",
                                      selectInput("Indicateur1", "Choose Indicator:", list("Single Indicator"=vars_1,
                                                                                           "Multilevel Regression - Level 2 Residuals"= vars_2), selected = "Residential Energy Consumption", selectize = FALSE),
                                      # Plot histogram inside absolute panel
                                      plotOutput("histogram", height = 250, width = 200)),
                                      
                        # Credits at bottom
                        tags$div(id="cite",
                                 HTML("Data Source : Phebus Survey (2013)<br>
                                Credits : Rémy Zumbiehl (CSTB, 2017)"
                                ))
                        
                    )
           ),

           # Tab Panel - Data Table  # 
           tabPanel("Phebus Data Table",
                    dataTableOutput("table")),
           
           # Tab Panel - PDF display  #### 
           tabPanel("Data Analysis PDF",
                    tags$iframe(style = " width:100% ; height:600px",  
                                src="These_Pro_Web.pdf"))
           
      )



#runApp(list(ui = ui, server = server),host="127.0.0.1",port=3696, launch.browser = T)



