library(shiny)
library(shinydashboard)
#library(shinyIncubator)
library(plyr)
library(dplyr)
library(leaflet)
library(rjson)
library(rgdal)
library(rCharts)
library(DT)

#setwd("/Users/admin/Desktop/Shiny/Phebus_data_analyse")


#Departement <- readOGR(dsn="Departement_phebus_ConsoE_RES2.shp",layer="OGRGeoJSON")  #stringsAsFactors=FALSE)



Departement <- readOGR(dsn="Departement_phebus_ConsoE_RES2_light.geojson",layer="OGRGeoJSON", stringsAsFactors=FALSE)

#Departement <- readOGR(dsn="/Users/admin/Desktop/Shiny/Phebus_data_analyse", layer= "Test_simplify_1207")

#Departement <- readOGR(".",layer="Departement_phebus_ConsoE_RES2") 
#Region <- readOGR(dsn="Region_phebus.geojson",layer="OGRGeoJSON", stringsAsFactors=FALSE)

#Region@data$ConsoE_MOY <- as.numeric(Region@data$ConsoE_MOY)
#Region@data$ConsoElec_MOY <- as.numeric(Region@data$ConsoElec_MOY)
#Region@data$DJU_region_2012 <- as.numeric(Region@data$DJU_region_2012)

shinyServer(function (input, output){
  

#~~~~~~~~~~~~~ REACTIVES ~~~~~~~~~~~~
  
  
  Dep_Poly <- reactive({
    if (input$Indicateur1 == "Residential Energy Consumption") {Departement@data$ConsoE_MOY}
    else if (input$Indicateur1 == "Residential Electricity Consumption") {Departement@data$ConsoElec_MOY}
    else if (input$Indicateur1 == "Heating Degree Day") {Departement@data$DJU_dep_2012}
    else if (input$Indicateur1 == "Mean Household Income") {Departement@data$REVDEP}
    else if (input$Indicateur1 == "Level 1 & 2 - All Variables") {Departement@data$RES2}
    else if (input$Indicateur1 == "Level 1 - Population Density") {Departement@data$RES2_LOGDENSPOP}
    else if (input$Indicateur1 == "Level 1 - Household Income") {Departement@data$RES2_LOGREV}
    else if (input$Indicateur1 == "Level 1 & 2 - Household Income") {Departement@data$RES2_LOGREV_LOGREVDEP}
    else if (input$Indicateur1 == "Level 1 & 2 - Rural zone and Population Density") {Departement@data$RES2_RURAL_LOGDENSPOP}
    })

#  Reg_Poly <- reactive({
#    if (input$Indicateur2 == "Energie Totale") {Region@data$ConsoE_MOY}
#    else if (input$Indicateur2 == "Electricity Consumption") {Region@data$ConsoElec_MOY}
#    else if (input$Indicateur2 == "DJU") {Region@data$DJU_region_2012}    
#  })
  
    
#~~~~~~~~~~~~~~  MAPS - DEPARTEMENTS ~~~~~~~~~~~~~~~ 
    output$map_departement <- renderLeaflet({
      
      ### POPUPS, PALETTES AND LEGENDS ###
      popup_dep <- reactive({
        if (input$Indicateur1 == "Residential Energy Consumption") {popup_Ener_dep}
        else if (input$Indicateur1 == "Residential Electricity Consumption") {popup_Elec_dep}
        else if (input$Indicateur1 == "Heating Degree Day") {popup_DJU_dep}
      })
       
      popup_Ener_dep <- paste0("<span style='color: #7f0000'><strong>Description Département :</strong></span>",
                                 "<br>", 
                                 "<br><span style='color: salmon;'><strong>Numero Département :  </strong></span>", 
                                 Departement@data$NumDep, 
                                 "<br><span style='color: salmon;'><strong>Nom du Département :  </strong></span>", 
                                 Departement@data$NOM_DEPT,
                                 "<br><span style='color: salmon;'><strong>Consommation d'Energie moyenne par ménage en 2012 :  </strong></span>", 
                                 Dep_Poly())
     
      popup_Elec_dep <- paste0("<span style='color: #7f0000'><strong>Description Département : </strong></span>",
                               "<br>", 
                               "<br><span style='color: salmon;'><strong>Numero Département :  </strong></span>", 
                               Departement@data$NumDep, 
                               "<br><span style='color: salmon;'><strong>Nom du Département :  </strong></span>", 
                               Departement@data$NOM_DEPT,
                               "<br><span style='color: salmon;'><strong>Consommation d'électricité moyenne par ménage en 2012 :  </strong></span>", 
                              Dep_Poly())
      
      popup_DJU_dep <- paste0("<span style='color: #7f0000'><strong>Description Département : </strong></span>",
                               "<br>", 
                               "<br><span style='color: salmon;'><strong>Numero Département :  </strong></span>", 
                               Departement@data$NumDep, 
                               "<br><span style='color: salmon;'><strong>Nom du Département :  </strong></span>", 
                               Departement@data$NOM_DEPT,
                               "<br><span style='color: salmon;'><strong>Degrés Jour Unifiés :  </strong></span>", 
                               Dep_Poly())
      
      palette_data_dep <- reactive({
        if (input$Indicateur1 == "Residential Energy Consumption") {palette_Ener_dep}
        else if (input$Indicateur1 == "Residential Electricity Consumption") {palette_Elec_dep}
        else if (input$Indicateur1 == "Heating Degree Day") {palette_DJU_dep}
        else if (input$Indicateur1 == "Mean Household Income") {palette_REVDEP}
        else if (input$Indicateur1 == "Level 1 & 2 - All Variables") {palette_RES2_dep}
        else if (input$Indicateur1 == "Level 1 - Population Density") {palette_RES2_dep}
        else if (input$Indicateur1 == "Level 1 - Household Income") {palette_RES2_dep}
        else if (input$Indicateur1 == "Level 1 & 2 - Household Income") {palette_RES2_dep}
        else if (input$Indicateur1 == "Level 1 & 2 - Rural zone and Population Density") {palette_RES2_dep}
      })
      
      palette_Ener_dep <- colorBin("Oranges", Dep_Poly(), bins = 5, pretty = TRUE) 
      palette_Elec_dep <- colorBin("Blues", Dep_Poly(), bins = 5, pretty = TRUE)
      palette_DJU_dep <- colorBin("RdYlBu", Dep_Poly(), bins = 10, pretty = TRUE)
      palette_REVDEP <- colorBin("Reds", Dep_Poly(), bins = 5, pretty = TRUE)
      palette_RES2_dep <- colorBin("RdYlGn", Dep_Poly(), bins = 5, reverse = TRUE, pretty = TRUE)
      
      legend_title_dep <- reactive({
        if (input$Indicateur1 == "Residential Energy Consumption") {legend_title_dep <- paste0("<span style='color: #000000'>Residential Energy<br>
                                                                                                     consumption in 2012 (kWh)</span>")} #span attributes ... : ;font-family: Monaco;font-size:0.875em
        else if (input$Indicateur1 == "Residential Electricity Consumption") {legend_title_dep <- paste0("<span style='color: #000000'><strong>Residential Electricity<strong><br>
                                                                                                     <strong>consumption in 2012 (kWh)</strong></span>")}
        else if (input$Indicateur1 == "Heating Degree Day") {legend_title_dep <- paste0("<span style='color: #000000'><strong>Heating Degree Days in 2012</strong><br>
                                                                                                     <strong>(Data source : ADEME)</strong></span>")}
        else if (input$Indicateur1 == "Mean Household Income") {legend_title_dep <- paste0("<span style='color: #000000'><strong>Mean Household Income</strong><br> 
                                                                                                  <strong>in 2012 (Euros) - Source: INSEE</strong><br></span>")}
                                                                                          #<span style='color: #000000'>Source: INSEE</span>")}
        else {legend_title_dep <- paste0("<span style='color: #000000'><strong>Level 2 Residuals<strong><br> 
                                         <strong>(Multilevel regression)</strong></span>")}
      })
      
      
      #### Progress Bar ############
      dat1 <- data.frame(x = numeric(0), y = numeric(0))
      withProgress(message = 'Creating Map', value = 0, {
        # Number of times we'll go through the loop
        n <- 20
        
        for (i in 1:n) {
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          dat1 <- rbind(dat1, data.frame(x = rnorm(1), y = rnorm(1)))
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Doing part", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.01)
        }
        
        
        plot(dat1$x, dat1$y)
      })
      
      
      
      ### AFFICHAGE CARTE / POPUPS / LEGENDES
      leaflet() %>% setView(lng = 4.20, lat = 46.30, zoom = 6) %>%
        addProviderTiles('Hydda.Base')  %>% 
        #addTiles() %>%
        addPolygons(data = Departement, 
                    weight= 2, 
                    fillColor = ~palette_data_dep()(Dep_Poly()), 
                    fillOpacity = 0.7, 
                    color = "white",
                    popup=popup_dep(),
                    highlightOptions = highlightOptions(
                      color='#f7f7f7', opacity = 2, weight = 4, fillOpacity = 1,
                      bringToFront = TRUE, sendToBack = TRUE)) %>%
        #addCircles(data = Departement, radius=10, fillColor = "black") %>%
        addLegend(position = 'bottomright', pal =  palette_data_dep(), values = Dep_Poly(), opacity = 0.6, labFormat = function(type, cuts, p) {n = length(cuts) 
        paste0(cuts[-n], " &ndash; ", cuts[-1])}, title = legend_title_dep())
      
      })  
    
    output$histogram <- renderPlot({
            hist(Dep_Poly(), freq = T, breaks = 30, col = "orange", xlab = "", ylab = "Count per Département", main = "Histogram")
        })
    
    # Pour la table des données
    output$table <- renderDataTable({datatable(Departement@data,rownames = FALSE,
                                    colnames = c("Departement Number","Departement Name","Residential Household Energy Consumption","Residential Household Electricity Consumption",
                                                 "Heating Degree Days (2012)","Level 2 Residuals - All Variables (levels 1 & 2)","Level 2 Residual- Population Density (level 2)",
                                                 "Level 2 Residuals - Household Income (level 1)","Level 2 Residuals - Household Income (level 1)",
                                                 "Level 2 Residuals - Rural zone and Population Density (level 1 & 2)","Logarithm of Average Household Income","Average Household Income"),
                                    options = list(
                                    pageLength = 50,
                                    scrollX=T,
                                    autoWidth = TRUE,
                                    columnDefs = list(list(width = "200px", targets = c(1,2,3)))))})

})    
    #~~~~~~~~~~~~~~  MAPS - REGIONS ~~~~~~~~~~~~~~~     
    
    
    # output$map_region <- renderLeaflet({
    #   
    #   ### POPUPS, PALETTES AND LEGENDS ###
    #   popup_reg <- reactive({
    #     if (input$Indicateur2 == "Energie Totale") {popup_Ener_reg}
    #     else if (input$Indicateur2 == "Electricité") {popup_Elec_reg}
    #     else if (input$Indicateur2 == "DJU") {popup_DJU_reg}
    #   })
    #   
    #   popup_Ener_reg <- paste0("<span style='color: #7f0000'><strong>Description Région :</strong></span>",
    #                            "<br>", 
    #                            "<br><span style='color: salmon;'><strong>Numero Région :  </strong></span>", 
    #                            Region@data$CODE_REG, 
    #                            "<br><span style='color: salmon;'><strong>Nom de la Région :  </strong></span>", 
    #                            Region@data$NOM_REG,
    #                            "<br><span style='color: salmon;'><strong>Consommation d'Energie moyenne par ménage en 2012 :  </strong></span>", 
    #                            Reg_Poly())
    #   
    #   popup_Elec_reg <- paste0("<span style='color: #7f0000'><strong>Description Région : </strong></span>",
    #                            "<br>", 
    #                            "<br><span style='color: salmon;'><strong>Numero Région :  </strong></span>", 
    #                            Region@data$CODE_REG, 
    #                            "<br><span style='color: salmon;'><strong>Nom de la Région :  </strong></span>", 
    #                            Region@data$NOM_REG,
    #                            "<br><span style='color: salmon;'><strong>Consommation d'électricité moyenne par ménage en 2012 :  </strong></span>", 
    #                            Reg_Poly())
    #   
    #   popup_DJU_reg <- paste0("<span style='color: #7f0000'><strong>Description Région : </strong></span>",
    #                           "<br>", 
    #                           "<br><span style='color: salmon;'><strong>Numero Région :  </strong></span>", 
    #                           Region@data$CODE_REG, 
    #                           "<br><span style='color: salmon;'><strong>Nom de la Région :  </strong></span>", 
    #                           Region@data$NOM_REG,
    #                           "<br><span style='color: salmon;'><strong>Degrés Jour Unifiés :  </strong></span>", 
    #                           Reg_Poly())
    #   
    #   palette_data_reg <- reactive({
    #     if (input$Indicateur2 == "Energie Totale") {palette_Ener_reg}
    #     else if (input$Indicateur2 == "Electricité") {palette_Elec_reg}
    #     else if (input$Indicateur2 == "DJU") {palette_DJU_reg}
    #   })
    #   
    #   palette_Ener_reg <- colorBin("Oranges", Reg_Poly(), bins = 5, pretty = TRUE) 
    #   palette_Elec_reg <- colorBin("Blues", Reg_Poly(), bins = 5, pretty = TRUE)
    #   palette_DJU_reg <- colorBin("Reds", Reg_Poly(), bins = 5, pretty = TRUE)
    #   
    #   legend_title_reg <- reactive({
    #     if (input$Indicateur2 == "Energie Totale") {legend_title_reg <- paste0("<span style='color: #7f0000'><strong>Energie Consommée<strong><br>
    #                                                                                                  <strong>par ménage en 2012 (kWh)</strong></span>")}
    #     else if (input$Indicateur2 == "Electricité") {legend_title_reg <- paste0("<span style='color: #7f0000'><strong>Electricité Consommée<strong><br>
    #                                                                                                  <strong>par ménage en 2012 (kWh)</strong></span>")}
    #     else if (input$Indicateur2 == "DJU") {legend_title_reg <- paste0("<span style='color: #7f0000'><strong>Dégrés Jour Unifiés en 2012</strong></span>")}
    #   })
    #   
    #   
    #   ### AFFICHAGE CARTE / POPUPS / LEGENDES
    #   leaflet() %>% setView(lng = 1.40, lat = 43.30, zoom = 5) %>%
    #     addProviderTiles('CartoDB.Positron')  %>% addTiles() %>%
    #     addPolygons(data = Region, 
    #                 weight= 2, 
    #                 fillColor = ~palette_data_reg()(Reg_Poly()), 
    #                 fillOpacity = 0.7, 
    #                 color = "white",
    #                 popup=popup_reg()) %>%
    #     addLegend(position = 'bottomright', pal =  palette_data_reg(), values = Reg_Poly(), opacity = 0.6, labFormat = function(type, cuts, p) {n = length(cuts) 
    #     paste0(cuts[-n], " &ndash; ", cuts[-1])}, title = legend_title_reg())
    #   
    # })  
    

    


 