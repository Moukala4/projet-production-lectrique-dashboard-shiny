library(shiny)
library(shinydashboard)
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
library("FactoMineR")
library("factoextra")
library("ggsci")
library(leaflet)
library(plotly)

ProElect <- read_excel("production.xlsx")
names(ProElect) <- gsub(" ", "", names(ProElect))

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Production électrique annuelle", titleWidth = 500),
  dashboardSidebar(
    selectInput("Année", "Sélectionner une Année:", choices = unique(ProElect$Année)),
    selectInput("Région", "Sélectionner une Région:", choices = unique(ProElect$Nomrégion)),
    selectInput("Département", "Sélectionner un Département:", choices = unique(ProElect$Nomdépartement)),
    selectInput("Commune", "Sélectionner une Commune:", choices = unique(ProElect$Nomcommune)),
    sidebarMenu(
      menuItem("Description", tabName = "description", icon = icon("info-circle")),
      menuItem("Analyse", tabName = "analyse", icon = icon("chart-line")),
      menuItem("Carte", tabName = "carte", icon = icon("map")),
      menuItem("Téléchargement", tabName = "download", icon = icon("download"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "description",
        h2("Description du projet"),
        p("Le jeu de données restitue la production électrique annuelle totale et le nombre de sites, par filière et par domaine de tension et de puissance à la maille commune sur le réseau géré par Enedis. Les données publiées correspondent aux années 2011 à 2021."),
        p("  ")
      ),
      tabItem(
        tabName = "analyse",
        fluidRow(
          box(
            title = "Sélectionnez la période",
            status = "primary",
            solidHeader = TRUE,
            sliderInput("slider", "Année:", min = 2011, max = 2022, value = c(2011, 2019))
          )
        ),
        fluidRow(
          box(
            title = "Graphique de Consommation",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot1")
          ),
          box(
            title = "Histogramme de Consommation",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot2")
          )
        )
      ),
      tabItem(
        tabName = "carte",
        tableOutput("clicked"),
        leafletOutput("map")
      ),
      tabItem(
        tabName = "download",
        fluidRow(
          box(
            title = "Téléchargez les Données",
            status = "primary",
            solidHeader = TRUE,
            downloadButton("downloadData", "Télécharger")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  data <- ProElect
  
  filteredData <- reactive({
    data %>%
      filter(Année >= input$slider[1], Année <= input$slider[2])
  })
  
  output$plot1 <- renderPlot({
    ggplot(filteredData(), aes(x = Année, y = EnergieproduiteannuelleAutresfilièresEnedis)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Consommation Électrique par Année", x = "Année", y = "production électrique annuelle")
  })
  
  output$plot2 <- renderPlot({
    ggplot(data, aes(x = EnergieproduiteannuelleAutresfilièresEnedis)) +
      geom_histogram(bins = 10, fill = "blue", alpha = 0.7) +
      labs(title = "Distribution de la Consommation Électrique", x = "Consommation", y = "Fréquence") +
      theme_minimal() +
      theme(
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "gray"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
