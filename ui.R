library(shiny)
library(shinydashboard)
library(DT)
library(plyr)
library(dplyr)
library(leaflet)
library(rjson)
library(rgdal)
library(rCharts)


library(leaflet)

# Choices for drop-downs
vars_1 <- c("Residential Energy Consumption", "Residential Electricity Consumption", "Heating Degree Day", "Mean Household Income")

vars_2 <- c("Level 1 & 2 - All Variables",
            "Level 1 - Population Density", "Level 1 - Household Income", "Level 1 & 2 - Household Income",
            "Level 1 & 2 - Rural zone and Population Density")

vars_region <- c("Energie Totale", "Electricité", "DJU")
title <- "Phebus Data Analyse"

navbarPage(windowTitle="Household Energy Consumption Data Analysis",
           title = div(tags$a(href = 'http://www.cstb.fr', 
                   img(src = 'logov4.png',
                         title = "CSTB", height = "50px"))),
           id="nav", inverse = FALSE,
                     
           
           tabPanel(
             
             "Map of various indicators per french Département division",
             includeCSS("styles.css"),
                    
             #title=tags$div(class = "navbar_header"),
                    div(class="outer",
                        #tags$head(
                        #includeCSS("styles.css")),
                        leafletOutput("map_departement", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 80, right = 10, #left = "auto",  bottom = "auto",
                                      width = 230, height = "auto",
                                      box(width = NULL, status = "warning", solidHeader = TRUE,
                                      #h2("Sélection : "),
                                      
                                      #selectInput("color", "Color", vars),
                                      selectInput("Indicateur1", "Choose Indicator:", list(
                                        "Single Indicator"=vars_1,
                                        "Multilevel Regression - Level 2 Residuals"= vars_2), selected = "Residential Energy Consumption", selectize = FALSE),
                                      plotOutput("histogram", height = 250, width = 200)))
             
                        ,
                                      
                        
                        tags$div(id="cite",
                                 HTML("Data Source : Phebus Survey (2013)<br>
                                Credits : Rémy Zumbiehl (CSTB, 2017)"
                                ))
                        
                    )
           ),
           tabPanel("Phebus Data Table",dataTableOutput("table")),
           
           tabPanel("Data Analysis PDF",
                    #pdf("www/These_Pro_v2.pdf"),
                    #htmlOutput("PDF"),
                    tags$iframe(style = "width:100% ; height:600px", src="These_Pro_v2.pdf"))#, width="900", height="600"))
           
           )



#runApp(list(ui = ui, server = server),host="127.0.0.1",port=3696, launch.browser = T)



