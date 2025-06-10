library(shiny)
library(sf)
library(ggplot2)
source("functions.R") 

shinyServer(function(input, output) {
  sf_data <- load_balanco_hidrico()
  
  # Gera o gráfico dinamicamente com base na seleção
  output$mapPlot <- renderPlot({
    ggplot(sf_data) +
      geom_sf(aes_string(fill = input$fill_var)) +
      theme_minimal() +
      labs(title = "Unidades de Balanço Hídrico",
           fill = input$fill_var) +
      theme(legend.position = "bottom")
  })
})
