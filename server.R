library(shiny)
library(shinydashboard)
library(shinyjs)
library(plyr)
library(dplyr)
library(leaflet)
library(rjson)
library(rgdal)
library(DT)
library(rgdal)
library(rgeos)
library(nlme)



###### Read csv and geojsons, rename columns ##########################################
Departement <- readOGR(dsn="Departement_phebus_ConsoE_RES2_light.geojson",layer="OGRGeoJSON", stringsAsFactors=FALSE)
Departement_model_1_gsimplify <- gSimplify(Departement, 0.01, topologyPreserve=TRUE)
colnames(Departement@data)[1] <- "DEP"
colnames(Departement@data)[2] <- "DEPNAME"


###### Multilevel Model using nlme package ##########################################
phebus13 <- read.csv("data_phebus_v16.csv")

model4_1 <- lme(LOGCONSTOT ~ #1 + 
                   LOGHDDDEP+
                   #LOGREVDEP+
                   LOGREV+
                   AREA3G+
                   INSULHOUS+
                   YEARCONST+
                   ROOMNBR+
                   HEATSYST+
                   HEATSOURCE+
                   RURAL+
                   HEATTEMP+
                   ECS+
                   UNOCCWEEK+
                   PCS+
                   NBRPERS,
                   random = ~ 1 | DEP,
                   data = phebus13)


###### Shiny Server ##################################################################

shinyServer(function (input, output){

  ### loading page ### 
  # Simulate work being done for 1 second
  Sys.sleep(1)
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  

  ### First tab reactives definition ###
  LOGHDDDEP_reactive_1 <- reactive({
      if (input$var1_LOGHDDDEP == "1300 DJU") {LOGHDDDEP <- c(7.17012)}
      else if (input$var1_LOGHDDDEP == "2000 DJU") {LOGHDDDEP <-c(7.600902)}
      else  {LOGHDDDEP <-c(8.039157)}})

  LOGREVDEP_reactive_2 <- reactive({
    if (input$var2_LOGREVDEP == "20 k€") {LOGREVDEP <-c(9.903488)}
    else if (input$var2_LOGREVDEP == "30 k€") {LOGREVDEP <-c(10.30895)}
    else  {LOGREVDEP <-c(10.59663)}})
  
  LOGREV_reactive_3 <- reactive({
    if (input$var3_LOGREV == "20 k€") {LOGREV <-c(9.903488)}
    else if (input$var3_LOGREV == "30 k€") {LOGREV <-c(10.30895)}
    else if (input$var3_LOGREV == "40 k€") {LOGREV <-c(10.59663)}
    else if (input$var3_LOGREV == "50 k€") {LOGREV <-c(10.81978)}
    else  {LOGREV <-c(11.0021)}})

  AREA3G_reactive_4 <- reactive({
    if (input$var4_AREA3G == "0-40m2") {AREA3G <-c("(0,40]")}
    else if (input$var4_AREA3G == "40-100m2") {AREA3G <-c("(40,100]")}
    else  {AREA3G <-c("(100,Inf]")}})
   
  INSULHOUS_reactive_5 <- reactive({
    if (input$var5_INSULHOUS == "Yes") {INSULHOUS <-c(0)}
    else  {INSULHOUS <-c(1)}})
   
  YEARCONST_reactive_6 <- reactive({
    if (input$var6_YEARCONST == "Between 1975 and 1990") {YEARCONST <-c("Between 1975 and 1990")}
    else if (input$var6_YEARCONST == "Before 1975") {YEARCONST <-c("Before 1975")}
    else  {YEARCONST <-c("After 1990")}})
   
  ROOMNBR_reactive_7 <- reactive({
    if (input$var7_ROOMNBR == "1") {ROOMNBR <-c(1)}
    else if (input$var7_ROOMNBR == "3") {ROOMNBR <-c(3)}
    else  {ROOMNBR <-c(5)}})
  
  HEATSYST_reactive_8 <- reactive({
    if (input$var8_HEATSYST == "Individual") {HEATSYST <-c("Indiv")}
    else if (input$var8_HEATSYST == "Collective") {HEATSYST <-c("COLL")}
    else  {HEATSYST <-c("MIXI")}})
   
  HEATSOURCE_reactive_9 <- reactive({
    if (input$var9_HEATSOURCE == "Electricity") {HEATSOURCE <-c("elec")}
    else if (input$var9_HEATSOURCE == "Gas") {HEATSOURCE <-c("gaz")}
    else  {HEATSOURCE <-c("autre")}})
  
  RURAL_reactive_10 <- reactive({
    if (input$var10_RURAL == "Yes") {RURAL <-c(1)}
    else  {RURAL <-c(0)}})
   
  HEATTEMP_reactive_11 <- reactive({
    if (input$var11_HEATTEMP == "Below 21°C") {HEATTEMP <-c(0)}
    else  {HEATTEMP <-c(1)}})
  
  ECS_reactive_12 <- reactive({
    if (input$var12_ECS == "Electricity Boiler") {ECS <-c("Electrique")}
    else if (input$var12_ECS == "Gas/Fuel/Wood Boiler") {ECS <-c("Chaudiere (G,F,B)")}
    else  {ECS <-c("Autres")}})
  
  UNOCCWEEK_reactive_13 <- reactive({
    if (input$var13_UNOCCWEEK == "< 4 hours per week") {UNOCCWEEK <-c(1)}
    else  {UNOCCWEEK <-c(0)}})
  
  PCS_reactive_14 <- reactive({
    if (input$var14_PCS == "Middle-level") {PCS <-c("Middle-level")}
    else if (input$var14_PCS == "Executive") {PCS <-c("Executive")}
    else  {PCS <-c("Other")}})
  
  NBRPERS_reactive_15 <- reactive({
    if (input$var15_NBRPERS == "1") {NBRPERS <-c(1)}
    else if (input$var15_NBRPERS == "2") {NBRPERS <-c(2)}
    else if (input$var15_NBRPERS == "3") {NBRPERS <-c(3)}
    else if (input$var15_NBRPERS == "4") {NBRPERS <-c(4)}
    else  {NBRPERS <-c(5)}})
  
  
  ### Mapping of MRM (Multilevel Regression Model) predictions ###
  output$MRM_Model <- renderLeaflet({
    
    # Data frame with all reactive parameters #
    predict_data <- data.frame(#LOGHDDDEP=LOGHDDDEP_reactive_1(),
                               LOGHDDDEP=unique(phebus13$LOGHDDDEP),
                               #LOGREVDEP=LOGREVDEP_reactive_2(),
                               #LOGREVDEP=unique(phebus13$LOGREVDEP),
                               LOGREV=LOGREV_reactive_3(),
                               AREA3G=AREA3G_reactive_4(),
                               INSULHOUS= INSULHOUS_reactive_5(),
                               YEARCONST=YEARCONST_reactive_6(),
                               ROOMNBR=ROOMNBR_reactive_7(),
                               HEATSYST=HEATSYST_reactive_8(),
                               HEATSOURCE=HEATSOURCE_reactive_9(),
                               RURAL=RURAL_reactive_10(),
                               HEATTEMP=HEATTEMP_reactive_11(),
                               ECS=ECS_reactive_12(),
                               UNOCCWEEK=UNOCCWEEK_reactive_13(),
                               PCS=PCS_reactive_14(),
                               NBRPERS=NBRPERS_reactive_15(),
                               DEP=unique(phebus13$DEP))
    
    # Two level Random-Intercept Modelling work and attach values to polygons
    pred <- predict(model4_1, predict_data,re.form=NULL) # Prediction with predict_data data frame
    pred1 <- data.frame(pred,unique(phebus13$DEP)) # New dataframe with predictions and DEP merged
    colnames(pred1)[2] <-"DEP" # Rename second column 
    pred1$DEP <- sprintf("%02d", as.numeric(pred1$DEP)) # Convert  1 to 01 (to avoid issues between 01 and 1, 02 and 2, etc..)
    Departement@data <- data.frame(Departement@data, pred1[match(Departement@data[,"DEP"], pred1[,"DEP"]),]) # Merge predictions with départements from geoJSON 
    Departement@data[c(4)] <- NULL  # Supress last column 
    Departement@data$pred <- round(exp(Departement@data$pred))  # Getting Exponential of logarithm of predictions
    Departement_model_1_SpatialDF <- SpatialPolygonsDataFrame(Departement_model_1_gsimplify,data=Departement@data) # Attach with polygons and create spatial data frame
    
    # Convert spatial data frame predictions in reactive 
    Departement_model_1_SpatialDF_reactive <- reactive({Departement_model_1_SpatialDF@data$pred}) 
    
    # Color Palette for prediction
    palette_Ener_pred_model <- colorBin("Oranges", Departement_model_1_SpatialDF_reactive(), bins = 5, pretty = TRUE) 
    
    # Pop-Ups
    popup_pred_model <- paste0("<span style='color: #7f0000'><strong>Description of Departement :</strong></span>",
                             "<br>", 
                             "<br><span style='color: salmon;'><strong>Id Number of Departement :  </strong></span>", 
                             Departement@data$DEP, 
                             "<br><span style='color: salmon;'><strong>Name of Departement :  </strong></span>", 
                             Departement@data$DEPNAME,
                             "<br><span style='color: salmon;'><strong>Household Energy Consumption predicted (kWh):  </strong></span>", 
                             Departement_model_1_SpatialDF_reactive())
    
    # Legende
    legend_pred_model <-  paste0("<span style='color: #000000'>Predicted Yearly<br>Household Energy<br>Consumption (kWh)</span>")
    
    
    # Progress Bar
    dat1 <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Creating Map', value = 0, {
      # Number of times we'll go through the loop
      n <- 40
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat1 <- rbind(dat1, data.frame(x = rnorm(1), y = rnorm(1)))
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
      plot(dat1$x, dat1$y)
    })
    
    # Map Building 
    leaflet() %>% setView(lng = -1, lat = 46.58, zoom = 6) %>% 
      addProviderTiles('Hydda.Base')  %>% 
      addPolygons(data = Departement, 
                  weight= 2, 
                  fillColor = ~palette_Ener_pred_model(Departement_model_1_SpatialDF_reactive()), 
                  fillOpacity = 0.7, 
                  color = "white",
                  popup=popup_pred_model,
                  highlightOptions = highlightOptions(
                                          color='#f7f7f7', 
                                          opacity = 2, 
                                          weight = 4, 
                                          fillOpacity = 1,
                                          bringToFront = TRUE, 
                                          sendToBack = TRUE)) %>%
      addLegend(position = 'bottomright', 
                pal =  palette_Ener_pred_model, 
                values = Departement_model_1_SpatialDF_reactive(), 
                opacity = 0.6, 
                labFormat = function(type, cuts, p) {n = length(cuts) 
                paste0(cuts[-n], " &ndash; ", cuts[-1])}, title = legend_pred_model)
    
  })

  ### Display abstract when clicking on button ###
  output$text <- renderUI({
    if (input$goButton %% 2){
    tags$iframe(src="ShinyApp_Text.pdf", height=300, width=500)
    } else {
          return()
        }  
  })
  
  
  ### Second tab reactives definition ###
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


 ### Mapping of MRM (Multilevel Regression Model) predictions ###
 output$map_departement <- renderLeaflet({
      
      # Convert Pop-Ups to reactive
      popup_dep <- reactive({
        if (input$Indicateur1 == "Residential Energy Consumption") {popup_Ener_dep}
        else if (input$Indicateur1 == "Residential Electricity Consumption") {popup_Elec_dep}
        else if (input$Indicateur1 == "Heating Degree Day") {popup_DJU_dep}
        else if (input$Indicateur1 == "Mean Household Income") {popup_income_dep}
      })
       
      # Pop-Ups
      popup_Ener_dep <- paste0("<span style='color: #7f0000'><strong>Description Département :</strong></span>",
                                 "<br>", 
                                 "<br><span style='color: salmon;'><strong>Numero Département :  </strong></span>", 
                                 Departement@data$DEP, 
                                 "<br><span style='color: salmon;'><strong>Nom du Département :  </strong></span>", 
                                 Departement@data$DEPNAME,
                                 "<br><span style='color: salmon;'><strong>Consommation d'Energie moyenne par ménage en 2012 :  </strong></span>", 
                                 Dep_Poly())
     
      popup_Elec_dep <- paste0("<span style='color: #7f0000'><strong>Description Département : </strong></span>",
                               "<br>", 
                               "<br><span style='color: salmon;'><strong>Numero Département :  </strong></span>", 
                               Departement@data$DEP, 
                               "<br><span style='color: salmon;'><strong>Nom du Département :  </strong></span>", 
                               Departement@data$DEPNAME,
                               "<br><span style='color: salmon;'><strong>Consommation d'électricité moyenne par ménage en 2012 :  </strong></span>", 
                              Dep_Poly())
      
      popup_DJU_dep <- paste0("<span style='color: #7f0000'><strong>Description Département : </strong></span>",
                               "<br>", 
                               "<br><span style='color: salmon;'><strong>Numero Département :  </strong></span>", 
                               Departement@data$DEP, 
                               "<br><span style='color: salmon;'><strong>Nom du Département :  </strong></span>", 
                               Departement@data$DEPNAME,
                               "<br><span style='color: salmon;'><strong>Degrés Jour Unifiés :  </strong></span>", 
                               Dep_Poly())
      
      popup_income_dep <- paste0("<span style='color: #7f0000'><strong>Description Département : </strong></span>",
                              "<br>", 
                              "<br><span style='color: salmon;'><strong>Numero Département :  </strong></span>", 
                              Departement@data$DEP, 
                              "<br><span style='color: salmon;'><strong>Nom du Département :  </strong></span>", 
                              Departement@data$DEPNAME,
                              "<br><span style='color: salmon;'><strong>Revenu Moyen du ménage dans le département :  </strong></span>", 
                              Dep_Poly())
      
      # Convert color palette to reactive
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
      
      # Color palette for all indicators
      palette_Ener_dep <- colorBin("Oranges", Dep_Poly(), bins = 5, pretty = TRUE) 
      palette_Elec_dep <- colorBin("Blues", Dep_Poly(), bins = 5, pretty = TRUE)
      palette_DJU_dep <- colorBin("RdYlBu", Dep_Poly(), bins = 10, pretty = TRUE)
      palette_REVDEP <- colorBin("Reds", Dep_Poly(), bins = 5, pretty = TRUE)
      palette_RES2_dep <- colorBin("RdYlGn", Dep_Poly(), bins = 5, reverse = TRUE, pretty = TRUE)
      
      # Convert legend to reactives
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
      
      
      # Another Progress Bar
      dat1 <- data.frame(x = numeric(0), y = numeric(0))
      withProgress(message = 'Creating Map', value = 0, {
        # Number of times we'll go through the loop
        n <- 40
        for (i in 1:n) {
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          dat1 <- rbind(dat1, data.frame(x = rnorm(1), y = rnorm(1)))
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Doing part", i))
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.01)}
        plot(dat1$x, dat1$y)
      })
      
      # Building indicator map
      leaflet() %>% setView(lng = 4.20, lat = 46.30, zoom = 6) %>%
        addProviderTiles('Hydda.Base')  %>% 
        addPolygons(data = Departement, 
                    weight= 2, 
                    fillColor = ~palette_data_dep()(Dep_Poly()), 
                    fillOpacity = 0.7, 
                    color = "white",
                    popup=popup_dep(),
                    highlightOptions = highlightOptions(
                                            color='#f7f7f7', 
                                            opacity = 2, 
                                            weight = 4, 
                                            fillOpacity = 1,
                                            bringToFront = TRUE, 
                                            sendToBack = TRUE)) %>%
        addLegend(position = 'bottomright', 
                  pal =  palette_data_dep(), 
                  values = Dep_Poly(), 
                  opacity = 0.6, 
                  labFormat = function(type, cuts, p) {n = length(cuts) 
                  paste0(cuts[-n], " &ndash; ", cuts[-1])}, 
                  title = legend_title_dep())
      })  
  
 ### Histogram ### 
  output$histogram <- renderPlot({
            hist(Dep_Poly(), 
                 freq = T, 
                 breaks = 30, 
                 col = "orange", 
                 xlab = "", ylab = "Département Count", 
                 main = "Histogram")
        })
    
 ### Data Table ###
    output$table <- renderDataTable({
                  datatable(phebus13[,-c(2,4,5,6,8,9,30,31,32)],
                                rownames = FALSE,
                                options = list(pageLength = 50,
                                               scrollX=T,
                                               autoWidth = TRUE,
                                               columnDefs = list(list(width = '200px', targets = c(2,5,7,8,9,10)))
                                    )
                            )
      })

})    
    
    


 