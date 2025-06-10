library(shiny)
library(ggplot2)

source("ui_vetorial.R")
source("ui_raster.R")

shinyUI(
  navbarPage(
      title = "Trabalho Final TDB - Manuela",
    vetorial_tab,
    raster_tab
  )
)
