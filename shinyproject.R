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
# Interface utilisateur
ui <- dashboardPage( skin="blue",
  dashboardHeader(title ="Production electrique annuelle",titleWidth = 500),

  dashboardSidebar(
    # Les filtres
    selectInput("Annee", "Selectionner une Annee:", choices = sort(unique(ProElect$Annee))),
    selectInput("Region", "Selectionner une Region:", choices = sort(unique(ProElect$Nomregion))),
    selectInput("Departement", "Selectionner une Departement:", choices = sort(unique(ProElect$Nomdepartement))),
    selectInput("Commune", "Selectionner une Commune:", choices = sort(unique(ProElect$Nomcommune))),
   
    
  sidebarMenu(
    menuItem("Description", tabName = "description", icon = icon("info-circle")),
    menuItem("Analyse", tabName = "analyse", icon = icon("chart-line")),
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
      
  
      
      tabItem(tabName = "analyse",
              tabBox(id="t1", width = 12, 

                     tabPanel("Photovoltaïque", plotOutput("energyRegionPlot"),plotOutput("energyYearPlot"),plotOutput("energyYearPlot"),plotOutput("correlationPlot"), icon=icon("chart-pie"),tableOutput("clusterEQ1_tab")),
                     tabPanel("Eolien",plotOutput("energyRegionPlot1"),plotOutput("energyYearPlot1"),plotOutput("energyYearPlot1"),plotOutput("correlationPlot1") ,icon=icon("uncharted"),tableOutput("clusterEQ2_tab")),
                     tabPanel("Hydraulique",plotOutput("energyRegionPlot2"),plotOutput("energyYearPlot2"),plotOutput("energyYearPlot2"),plotOutput("correlationPlot2") ,icon=icon("chart-pie"),tableOutput("clusterEQ2_tab")),
                     tabPanel("Bio Energie", plotOutput("energyRegionPlot3"),plotOutput("energyYearPlot3"),plotOutput("energyYearPlot3"),plotOutput("correlationPlot3"),icon=icon("chart-pie"),tableOutput("clusterEQ2_tab")),
                     tabPanel("Cogeneration",plotOutput("energyRegionPlot4"),plotOutput("energyYearPlot4"),plotOutput("energyYearPlot4"),plotOutput("correlationPlot4") ,icon=icon("chart-pie"),tableOutput("clusterEQ2_tab")),
                     tabPanel("Autres Filières",plotOutput("energyRegionPlot5"),plotOutput("energyYearPlot5"),plotOutput("energyYearPlot5"),plotOutput("correlationPlot5") ,icon=icon("chart-pie"),tableOutput("clusterEQ2_tab")),
                  
                     tabPanel("Conclusion", htmlOutput("conclusion"), icon=icon("chart-pie")))
      ),
      
      tabItem(tabName = "carte",tableOutput("clicked"),leafletOutput("map")),
      
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
  
server <- function(input, output) {
    
    data <- ProElect
    
    observeEvent(input$Annee, {
      updateSelectInput(session = session, inputId = "Annee",label = "Annee",
                        choices = unique(filtereddata()$Annee), selected = input$Annee)
    })
    # Le filtre des clubs
    filtered_data <- reactive({
      df <- subset(filtereddata(), Club == input$Club)
      return(df)
    })

      # Filtre les donnees en fonction des choix de l'utilisateur
      filteredData <- reactive({
        data %>%
          filter(data$Annee == input$Annee | is.null(input$Annee),
                 data$Nomregion== input$Region | is.null(input$Region),
                 data$Nomdepartement == input$Departement | is.null(input$Departement),
                 data$Nomcommune == input$Commune | is.null(input$Commune))
      })
      
      # Repartition de l'energie photovoltaïque par region
      output$energyRegionPlot <- renderPlot({
        ProElect %>%
          group_by(`Nomregion`) %>%
          summarise(Total = sum(data$EnergieproduiteannuellePhotovoltaïqueEnedis.MWh., na.rm = TRUE)) %>%
          ggplot(aes(x = data$Nomregion, y = Total, fill = data$Nomregion)) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(title = "Production photovoltaïque par region",
               x = "Region",
               y = "Production totale (MWh)")
      })
      
      # evolution annuelle de la production photovoltaïque
      output$energyYearPlot <- renderPlot({
        filter() %>%
          group_by(data$Annee) %>%
          summarise(Total = sum(data$EnergieproduiteannuellePhotovoltaïqueEnedis.MWh., na.rm = TRUE)) %>%
          ggplot(aes(x = data$Annee, y = Total)) +
          geom_line() +
          labs(title = "evolution annuelle de la production photovoltaïque",
               x = "Annee",
               y = "Production totale (MWh)")
      })
      
      # Carte de production par commune (n'oubliez pas d'ajuster la logique pour les coordonnees)
      output$energyMap <- renderLeaflet({
        # Vous devrez transformer 'centroid' en coordonnees utilisables et filtrer les donnees
        # ...
      })
      
      # Correlation entre le nombre de sites et la production d'energie
      output$correlationPlot <- renderPlot({
        filter() %>%
          ggplot(aes(x = data$NbsitesPhotovoltaïqueEnedis, y = data$EnergieproduiteannuellePhotovoltaïqueEnedis.MWh.)) +
          geom_point() +
          geom_smooth(method = lm) +
          labs(title = "Correlation entre le nombre de sites et la production photovoltaïque",
               x = "Nombre de sites",
               y = "Production (MWh)")
      })
      
      # Vous pouvez ajouter d'autres graphiques ici en suivant un schema similaire
    
    
    
    
    
    
    
    
    
    
    output$plot1 <- renderPlot({
      ggplot(filter(), aes(x =Annee , y = EnergieproduiteannuelleAutresfilièresEnedis(MWh))) +
        geom_line() +
        theme_minimal() +
        labs(title = "Consommation electrique par Annee", x = "Annee", y = "production electrique annuelle")
    })
    output$plot2 <- renderPlot({
      ggplot(data, aes(x =EnergieproduiteannuelleAutresfilièresEnedis(MWh) )) +
        geom_histogram(bins = 10, fill = "blue", alpha = 0.7) +
        labs(title = "Distribution de la Consommation electrique", x = "Consommation", y = "Frequence") +
        theme_minimal() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_line(colour = "gray"),
              panel.grid.minor = element_blank())
    })
    # Telechargement des donnees
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filteredData(), file, row.names = FALSE)
      }
    )
  }
  
  # Execute l'application
  shinyApp(ui, server)
  