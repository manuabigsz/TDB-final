library(shiny)
library(sf)
library(ggplot2)
library(raster)
library(DT)
library(leaflet)
library(DBI)
library(RPostgres)
library(plotly)

source("functions.R")
source("server_vet.R")
source("server_raster.R")

shinyServer(function(input, output, session) {
  vetorialServer(input, output, session)
  rasterServer(input, output, session)
})
