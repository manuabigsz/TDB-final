library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  titlePanel("Mapa das Unidades de Balanço Hídrico"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("fill_var", "Cor por:", choices = c("bacia", "upg", "id_balanco"), selected = "bacia")
    ),
    
    mainPanel(
      plotOutput("mapPlot", height = "700px")
    )
  )
))