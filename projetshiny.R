

##################################################################################
#                               R SHINY                                          #
#                                                                                #
##################################################################################
library(shiny)
library(shinydashboard)
library(shinythemes)
library(scales)
library(forcats)
library(wrapr)
library(gridExtra)
library(ggplot2movies)
library(tidyverse)
library(rlang)
library(DT)
library(plyr)
library(ggplot2)
library(ggnewscale)
library(stringr)
library(ggh4x)
library(dplyr)
library(readxl)
library("ggsci")
library(leaflet)
library(plotly)


ProElect <- read_excel("production.xlsx")
names(ProElect) <- gsub(" ", "", names(ProElect))
ProElect<-data.frame(ProElect)
# Supposons que ProElect$centroid est la colonne que nous voulons diviser
# D'abord, nous allons separer les chaînes à la virgule
split_centroid <- strsplit(as.character(ProElect$centroid), ", ")

# Ensuite, nous creons deux nouvelles colonnes dans ProElect
ProElect$Longitude <- sapply(split_centroid, function(x) as.numeric(x[1]))
ProElect$Latitude <- sapply(split_centroid, function(x) ifelse(length(x) > 1, as.numeric(x[2]), NA))

# Remplacer les valeurs NA par une valeur par defaut si necessaire
# ProElect$Longitude[is.na(ProElect$Longitude)] <- valeur_par_defaut_longitude
# ProElect$Latitude[is.na(ProElect$Latitude)] <- valeur_par_defaut_latitude

ProElect <- data.frame(lapply(ProElect, function(x) ifelse(is.na(x), 0, x)))

# Interface utilisateur
ui <- dashboardPage( skin="blue",
                     dashboardHeader(title ="Production electrique annuelle",titleWidth = 500),
                     
                     dashboardSidebar(
                       # Les filtres
                       selectInput("Annee", "Selectionner une Annee:", choices = sort(unique(ProElect$Annee))),
                       selectInput("Region", "Selectionner une Region:", choices = sort(unique(ProElect$Nomregion))),
                       selectInput("Departement", "Selectionner une Departement:", choices = sort(unique(ProElect$Nomdepartement))),
      
                       
                       
                       sidebarMenu(
                         menuItem("Description", tabName = "description", icon = icon("info-circle")),  
                         menuItem("Photovoltaïque", tabName = "Photovoltaïque", icon = icon("sun")),  
                         menuItem("Eolien", tabName = "Eolien", icon = icon("wind")),  
                         menuItem("Hydraulique", tabName = "Hydraulique", icon = icon("water")),  
                         menuItem("Bio Energie", tabName = "BioEnergie", icon = icon("leaf")), 
                         menuItem("Cogeneration", tabName = "Cogeneration", icon = icon("industry")),  
                         menuItem("Autres Filières", tabName = "AutreFilières", icon = icon("bolt")),  
                         menuItem("Carte", tabName = "carte", icon = icon("map")), 
                         menuItem("Telechargement", tabName = "download", icon = icon("download"))  
                         
                         
                       )
                     ),
                     
                     dashboardBody(
                       tabItems(
                         tabItem(
                           tabName = "description",
                           h2("Description du projet"),
                           p("Le jeu de donnees restitue la production electrique annuelle totale et le nombre de sites, par filière et par domaine de tension et de puissance à la maille commune sur le reseau gere par Enedis. Les donnees publiees correspondent aux annees 2011 à 2021."),
                           p("  ")
                         ),
                         
                         
                         
                         # tabItem(tabName = "analyse",
                         #         tabBox( width = 12, 
                         #                
                         #                tabPanel("Photovoltaïque", plotOutput("energyRegionPlot"),plotOutput("energyYearPlot"),plotOutput("energyYearPlot"),plotOutput("correlationPlot"), icon=icon("chart-pie"),tableOutput("clusterEQ1_tab")),
                         #                tabPanel("",plotOutput("energyRegionPlot1"),plotOutput("energyYearPlot1"),plotOutput("energyYearPlot1"),plotOutput("correlationPlot1") ,icon=icon("uncharted"),tableOutput("clusterEQ2_tab")),
                         #                tabPanel("",plotOutput("energyRegionPlot2"),plotOutput("energyYearPlot2"),plotOutput("energyYearPlot2"),plotOutput("correlationPlot2") ,icon=icon("chart-pie"),tableOutput("clusterEQ2_tab")),
                         #                tabPanel("", plotOutput("energyRegionPlot3"),plotOutput("energyYearPlot3"),plotOutput("energyYearPlot3"),plotOutput("correlationPlot3"),icon=icon("chart-pie"),tableOutput("clusterEQ2_tab")),
                         #                tabPanel("",plotOutput("energyRegionPlot4"),plotOutput("energyYearPlot4"),plotOutput("energyYearPlot4"),plotOutput("correlationPlot4") ,icon=icon("chart-pie"),tableOutput("clusterEQ2_tab")),
                         #                tabPanel("",plotOutput("energyRegionPlot5"),plotOutput("energyYearPlot5"),plotOutput("energyYearPlot5"),plotOutput("correlationPlot5") ,icon=icon("chart-pie"),tableOutput("clusterEQ2_tab")),
                         #                
                         #                tabPanel("Conclusion", htmlOutput("conclusion"), icon=icon("chart-pie")))
                         # ),

# photoV ------------------------------------------------------------------

                         
                         tabItem(tabName = "Photovoltaïque",
                                 fluidRow(
                                   box(title = "Graphique de Production par annee", status = "primary", solidHeader = TRUE,
                                       plotOutput("energyYearPlot")
                                   ),
                                   box(title = "Graphique de Production par region ", status = "primary", solidHeader = TRUE,
                                       plotOutput("energyRegionPlot")
                                   ),
                                   box(title = "Graphique de Production par Département ", status = "primary", solidHeader = TRUE,
                                       plotOutput("energydepPlot")
                                   )
                                 )),
                         

# Eolien ------------------------------------------------------------------
tabItem(tabName = "Eolien",
        fluidRow(
          box(title = "Graphique de Production par annee", status = "primary", solidHeader = TRUE,
              plotOutput("energyYearPlot1")
          ),
          box(title = "Graphique de Production par region ", status = "primary", solidHeader = TRUE,
              plotOutput("energyRegionPlot1")
          ),
          box(title = "Graphique de Production par Département ", status = "primary", solidHeader = TRUE,
              plotOutput("energydepPlot1")
          )
        )),

# hydraulique -------------------------------------------------------------
tabItem(tabName = "Hydraulique",
        fluidRow(
          box(title = "Graphique de Production par annee", status = "primary", solidHeader = TRUE,
              plotOutput("energyYearPlot2")
          ),
          box(title = "Graphique de Production par region ", status = "primary", solidHeader = TRUE,
              plotOutput("energyRegionPlot2")
          ),
          box(title = "Graphique de Production par Département ", status = "primary", solidHeader = TRUE,
              plotOutput("energydepPlot2")
          )
        )),

# Bio Energie -------------------------------------------------------------

tabItem(tabName = "BioEnergie",
        fluidRow(
          box(title = "Graphique de Production par annee", status = "primary", solidHeader = TRUE,
              plotOutput("energyYearPlot3")
          ),
          box(title = "Graphique de Production par region ", status = "primary", solidHeader = TRUE,
              plotOutput("energyRegionPlot3")
          ),
          box(title = "Graphique de Production par Département ", status = "primary", solidHeader = TRUE,
              plotOutput("energydepPlot3")
          )
        )),
# congeneration -----------------------------------------------------------

tabItem(tabName = "Cogeneration",
        fluidRow(
          box(title = "Graphique de Production par annee", status = "primary", solidHeader = TRUE,
              plotOutput("energyYearPlot4")
          ),
          box(title = "Graphique de Production par region ", status = "primary", solidHeader = TRUE,
              plotOutput("energyRegionPlot4")
          ),
          box(title = "Graphique de Production par Département ", status = "primary", solidHeader = TRUE,
              plotOutput("energydepPlot4")
          )
        )),
# Autres filières ---------------------------------------------------------


tabItem(tabName = "AutreFilières",
        fluidRow(
          box(title = "Graphique de Production par annee", status = "primary", solidHeader = TRUE,
              plotOutput("energyYearPlot5")
          ),
          box(title = "Graphique de Production par region ", status = "primary", solidHeader = TRUE,
              plotOutput("energyRegionPlot5")
          ),
          box(title = "Graphique de Production par Département ", status = "primary", solidHeader = TRUE,
              plotOutput("energydepPlot5")
          )
        )),                         

# CARTE -------------------------------------------------------------------                        
                         
                         tabItem(tabName = "carte",tableOutput("clicked"),leafletOutput("map")),


# TELECHARGEMENT -------------------------------------------------------------------
                         
                         tabItem(tabName = "download",
                                 fluidRow(
                                   box(title = "Telechargez les Donnees", status = "primary", solidHeader = TRUE,
                                       downloadButton("downloadData", "Telecharger")
                                   )
                                 )
                         )
                       )
                     )
                     
)
# Serveur
server <- function(input, output) {



  # ne marche pas mais sera amélioré
  # # Observer pour mettre à jour les choix de Région
  # observe({
  #   # Supposons que vous souhaitez que les régions soient filtrées en fonction de l'année sélectionnée
  #   filtered_regions <- ProElect %>%
  #     filter(Annee == input$Annee) %>%
  #     pull(Nomregion) %>%
  #     unique() %>%
  #     sort()
  #   
  #   # Mettre à jour les choix de Région dans l'interface utilisateur
  #   updateSelectInput(session, "Region",
  #                     choices = c("Toutes les régions", filtered_regions),
  #                     selected = "Toutes les régions")
  # })
  # 
  # # Observer pour mettre à jour les choix de Département en fonction de la Région sélectionnée
  # observe({
  #   # Filtrer les données en fonction de la région sélectionnée
  #   filtered_departments <- ProElect %>%
  #     filter(Annee == input$Annee, Nomregion == input$Region) %>%
  #     pull(Nomdepartement) %>%
  #     unique() %>%
  #     sort()
  #   
  #   # Mettre à jour les choix de Département dans l'interface utilisateur
  #   updateSelectInput(session, "Departement",
  #                     choices = c("Tous les départements", filtered_departments),
  #                     selected = "Tous les départements")
  # })
  
 
 

# ANALYSE -----------------------------------------------------------------

 
  
##################################photovoltaique#############################  
  
 
  

  # graphique pour l'analyse par region
  
  output$energyRegionPlot <- renderPlot({
  
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuellePhotovoltaïqueEnedis.MWh., na.rm = TRUE))

    ggplot(ProElect, aes(x = Nomregion, y = EnergieproduiteannuellePhotovoltaïqueEnedis.MWh., fill = Nomregion)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Région", y = "Production") +
      theme(legend.position = "none")  
  })


  output$energyYearPlot <- renderPlot({
  
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuellePhotovoltaïqueEnedis.MWh., na.rm = TRUE))

  
    ggplot(aggregatedData, aes(x = Annee, y = TotalProduction, group = 1)) +
      geom_line(color = "blue") +  
      geom_point(color = "red") +  
      theme_minimal() +
      labs(title = "Production électrique par année", x = "Année", y = "Production Totale")
  })

  
  # graphique pour l'analyse par Département
  output$energydepPlot <- renderPlot({
    # Summarize the data to get the sum of electric production per year
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuellePhotovoltaïqueEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomdepartement, y = EnergieproduiteannuellePhotovoltaïqueEnedis.MWh., fill = Nomdepartement)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Département", y = "Production") +
      theme(legend.position = "none")  
  })
 
  



  #######################################################Eolien#############################
  # graphique pour l'analyse par region
  
  output$energyRegionPlot1 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleEolienEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomregion, y = EnergieproduiteannuelleEolienEnedis.MWh., fill = Nomregion)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Région", y = "Production") +
      theme(legend.position = "none")  
  })
  
  
  output$energyYearPlot1 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleEolienEnedis.MWh., na.rm = TRUE))
    
    
    ggplot(aggregatedData, aes(x = Annee, y = TotalProduction, group = 1)) +
      geom_line(color = "blue") +  
      geom_point(color = "red") +  
      theme_minimal() +
      labs(title = "Production électrique par année", x = "Année", y = "Production Totale")
  })
  
  
  # graphique pour l'analyse par Département
  output$energydepPlot1 <- renderPlot({
    # Summarize the data to get the sum of electric production per year
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleEolienEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomdepartement, y = EnergieproduiteannuelleEolienEnedis.MWh., fill = Nomdepartement)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Département", y = "Production") +
      theme(legend.position = "none")  
  })
  
  
  

  #######################################################Hydraulique#############################
  
  # graphique pour l'analyse par region
  
  output$energyRegionPlot2 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleHydrauliqueEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomregion, y = EnergieproduiteannuelleHydrauliqueEnedis.MWh., fill = Nomregion)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Région", y = "Production") +
      theme(legend.position = "none")  
  })
  
  
  output$energyYearPlot2 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleHydrauliqueEnedis.MWh., na.rm = TRUE))
    
    
    ggplot(aggregatedData, aes(x = Annee, y = TotalProduction, group = 1)) +
      geom_line(color = "blue") +  
      geom_point(color = "red") +  
      theme_minimal() +
      labs(title = "Production électrique par année", x = "Année", y = "Production Totale")
  })
  
  
  # graphique pour l'analyse par Département
  output$energydepPlot2 <- renderPlot({
    # Summarize the data to get the sum of electric production per year
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleHydrauliqueEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomdepartement, y = EnergieproduiteannuelleHydrauliqueEnedis.MWh., fill = Nomdepartement)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Département", y = "Production") +
      theme(legend.position = "none")  
  })
  
  
  
  
  
 
  #######################################################Bio Energie#############################
  # graphique pour l'analyse par region
  
  output$energyRegionPlot3 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleBioEnergieEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomregion, y = EnergieproduiteannuelleBioEnergieEnedis.MWh., fill = Nomregion)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Région", y = "Production") +
      theme(legend.position = "none")  
  })
  
  
  output$energyYearPlot3 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleBioEnergieEnedis.MWh., na.rm = TRUE))
    
    
    ggplot(aggregatedData, aes(x = Annee, y = TotalProduction, group = 1)) +
      geom_line(color = "blue") +  
      geom_point(color = "red") +  
      theme_minimal() +
      labs(title = "Production électrique par année", x = "Année", y = "Production Totale")
  })
  
  
  # graphique pour l'analyse par Département
  output$energydepPlot3 <- renderPlot({
    # Summarize the data to get the sum of electric production per year
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleBioEnergieEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomdepartement, y = EnergieproduiteannuelleBioEnergieEnedis.MWh., fill = Nomdepartement)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Département", y = "Production") +
      theme(legend.position = "none")  
  })
  
  
  
  
  
  
  
  
  #######################################################Cogeneration#############################
  # graphique pour l'analyse par region
  
  output$energyRegionPlot4 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleAutresfilièresEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomregion, y = EnergieproduiteannuelleCogenerationEnedis.MWh., fill = Nomregion)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Région", y = "Production") +
      theme(legend.position = "none")  
  })
  
  
  output$energyYearPlot4 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleCogenerationEnedis.MWh., na.rm = TRUE))
    
    
    ggplot(aggregatedData, aes(x = Annee, y = TotalProduction, group = 1)) +
      geom_line(color = "blue") +  
      geom_point(color = "red") +  
      theme_minimal() +
      labs(title = "Production électrique par année", x = "Année", y = "Production Totale")
  })
  
  
  # graphique pour l'analyse par Département
  output$energydepPlot4 <- renderPlot({
    # Summarize the data to get the sum of electric production per year
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleCogenerationEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomdepartement, y = EnergieproduiteannuelleBioEnergieEnedis.MWh., fill = Nomdepartement)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Département", y = "Production") +
      theme(legend.position = "none")  
  })
  
  
  
  #######################################################Autres Filières#############################

  # graphique pour l'analyse par region
  
  output$energyRegionPlot5 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleAutresfilièresEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomregion, y = EnergieproduiteannuelleAutresfilièresEnedis.MWh., fill = Nomregion)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Région", y = "Production") +
      theme(legend.position = "none")  
  })
  
  
  output$energyYearPlot5 <- renderPlot({
    
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleAutresfilièresEnedis.MWh., na.rm = TRUE))
    
    
    ggplot(aggregatedData, aes(x = Annee, y = TotalProduction, group = 1)) +
      geom_line(color = "blue") +  
      geom_point(color = "red") +  
      theme_minimal() +
      labs(title = "Production électrique par année", x = "Année", y = "Production Totale")
  })
  
  
  # graphique pour l'analyse par Département
  output$energydepPlot5 <- renderPlot({
    # Summarize the data to get the sum of electric production per year
    aggregatedData <- ProElect %>%
      group_by(Annee) %>%
      summarise(TotalProduction = sum(EnergieproduiteannuelleAutresfilièresEnedis.MWh., na.rm = TRUE))
    
    ggplot(ProElect, aes(x = Nomdepartement, y = EnergieproduiteannuelleBioEnergieEnedis.MWh., fill = Nomdepartement)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1") +  
      theme_minimal() +
      labs(title = "Production électrique par région", x = "Département", y = "Production") +
      theme(legend.position = "none")  
  })
  
  #######################################################Conclusion#############################






  ###################################################################
  ###############################CARTE###############################
  ###################################################################
  # carte interactive 
  output$carte <- renderLeaflet({
    leaflet(ProElect) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = ~Nomcommune)
  })


  # output$map=renderLeaflet({
  #
  #   ProElect$Latitude <- sapply( ProElect$Latitude , as.numeric)
  #   ProElect$Longitude <- sapply(ProElect$Longitude  , as.numeric)
  #   latitude<-df$Latitude
  #   logitude<-df$Longitude
  #   map <- leaflet(df) %>% addTiles()
  #   map <- map %>%addMarkers(lng = ~ Longitude, lat = ~ Latitude,popup = ~fini$Club...2,
  #                            layerId = ~fini$id,clusterOptions = markerClusterOptions(),clusterId = "points")
  #   map
  #   map<-map
  # })
  # observeEvent(input$map_marker_click,{
  #   print(input$map_marker_click)
  #   output$clicked=renderTable(data.frame(input$map_marker_click))
  # })


  ###################################################################
  ########################TELECHARGEMENT#############################
  ###################################################################
  # Exemple de sortie pour telechargement de donnees
  output$downloadData <- downloadHandler(
    filename = function() { paste("data-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      write.csv(ProElect, file)
    }
  )
  
  # d'autres reactivites et sorties Serons ajoutés
}

# Executer l'application
shinyApp(ui, server)
