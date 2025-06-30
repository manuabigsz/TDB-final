library(leaflet)
vetorialServer <- function(input, output, session) {
  
  sf_data <- load_balanco_hidrico() 
  
  observe({
    if (input$fill_var == "upg") {
      upg_choices <- sort(unique(sf_data$upg))
      output$upgSelectUI <- renderUI({
        selectInput("selected_upg", "Selecionar UPG específica:", 
                    choices = upg_choices, selected = upg_choices[1])
      })
    } else {
      output$upgSelectUI <- renderUI({ NULL })
    }
  })
  
  output$mapPlot <- renderLeaflet({
    leaflet(sf_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        layerId = ~as.character(id_balanco),
        fillColor = "lightblue",
        fillOpacity = 0.5,
        color = "white",
        weight = 1,
        label = ~paste("Bacia:", bacia, "<br>UPG:", upg, "<br>ID Balanço:", id_balanco),
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7)
      )
  })
  
  observe({
    if (input$fill_var == "bacia") {
      pal <- colorFactor(palette = "Set3", domain = sf_data$bacia)
      
      leafletProxy("mapPlot", data = sf_data) %>%
        clearShapes() %>%
        addPolygons(
          fillColor = ~pal(bacia),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          label = ~paste("Bacia:", bacia, "<br>UPG:", upg, "<br>ID Balanço:", id_balanco),
          highlightOptions = highlightOptions(weight = 3, color = "#444", fillOpacity = 0.9)
        )
    } else if (input$fill_var == "upg") {
      req(input$selected_upg)
      leafletProxy("mapPlot", data = sf_data) %>%
        clearShapes() %>%
        addPolygons(
          data = sf_data %>% filter(upg != input$selected_upg),
          fillColor = "lightgray",
          fillOpacity = 0.4,
          color = "white",
          weight = 1,
          label = ~paste("Bacia:", bacia, "<br>UPG:", upg, "<br>ID Balanço:", id_balanco),
          highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.5)
        ) %>%
        addPolygons(
          data = sf_data %>% filter(upg == input$selected_upg),
          fillColor = "blue",
          fillOpacity = 0.7,
          color = "white",
          weight = 2,
          label = ~paste("Bacia:", bacia, "<br>UPG:", upg, "<br>ID Balanço:", id_balanco),
          highlightOptions = highlightOptions(weight = 3, color = "#222", fillOpacity = 0.8)
        )
    } else {
      leafletProxy("mapPlot", data = sf_data) %>%
        clearShapes() %>%
        addPolygons(
          fillColor = "lightblue",
          fillOpacity = 0.5,
          color = "white",
          weight = 1,
          label = ~paste("Bacia:", bacia, "<br>UPG:", upg, "<br>ID Balanço:", id_balanco),
          highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.7)
        )
    }
  })
  
  
  observe({
    if (input$fill_var == "upg") {
      req(input$selected_upg)
      selected_sf <- sf_data %>% filter(upg == input$selected_upg)
      
      leafletProxy("mapPlot", data = sf_data) %>%
        clearShapes() %>%
        addPolygons(
          data = sf_data %>% filter(upg != input$selected_upg),
          fillColor = "lightgray",
          fillOpacity = 0.4,
          color = "white",
          weight = 1,
          label = ~paste("Bacia:", bacia, "<br>UPG:", upg, "<br>ID Balanço:", id_balanco),
          highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.5)
        ) %>%
        addPolygons(
          data = selected_sf,
          fillColor = "blue",
          fillOpacity = 0.7,
          color = "white",
          weight = 2,
          label = ~paste("Bacia:", bacia, "<br>UPG:", upg, "<br>ID Balanço:", id_balanco),
          highlightOptions = highlightOptions(weight = 3, color = "#222", fillOpacity = 0.8)
        )
    }
  })
  
  clicked_region <- reactiveVal(NULL)
  
  observeEvent(input$mapPlot_shape_click, {
    click <- input$mapPlot_shape_click
    clicked_id <- click$id
    
    if (!is.null(clicked_id)) {
      selected <- sf_data[sf_data$id_balanco == clicked_id, ]
      clicked_region(selected) 
      
      bbox <- sf::st_bbox(selected)
      leafletProxy("mapPlot") %>%
        flyToBounds(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
      
      output$clickInfo <- renderUI({
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #007bff;",
          h4("Informações da Região Selecionada", style = "color: #007bff; margin-top: 0;"),
          p(strong("Bacia: "), selected$bacia %||% "Não informado"),
          p(strong("UPG: "), selected$upg %||% "Não informado"),
          p(strong("ID Balanço: "), selected$id_balanco %||% "Não informado")
        )
      })
    }
  })
  
  output$barPlot <- renderPlot({
    region <- clicked_region()
    
    if (is.null(region)) {
      areas <- sf::st_area(sf_data)
      sf_data$area_km2 <- as.numeric(areas) / 10^6
      agg <- sf_data %>%
        group_by(upg) %>%
        summarise(area_km2 = sum(area_km2, na.rm = TRUE)) %>%
        arrange(desc(area_km2))
      
      barplot(agg$area_km2,
              names.arg = agg$upg,
              las = 2,
              col = "darkcyan",
              main = "Área Total por UPG (km²)",
              ylab = "Área (km²)",
              cex.names = 0.7)
    } else {
      area <- sf::st_area(region)
      barplot(as.numeric(area) / 10^6,
              names.arg = region$upg,
              col = "steelblue", 
              main = "Área da Região Selecionada (km²)",
              ylab = "Área (km²)")
    }
  })
  
  
  output$sfTable <- DT::renderDataTable({
    region <- clicked_region()
    
    if (is.null(region)) {
      DT::datatable(sf::st_drop_geometry(sf_data),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    } else {
      DT::datatable(sf::st_drop_geometry(region),
                    options = list(pageLength = 5),
                    rownames = FALSE)
    }
  })
  
  output$clickInfo <- renderUI({
    div(
      style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; border-left: 4px solid #17a2b8;",
      p("Clique em uma região do mapa para ver as informações da Bacia e UPG.", 
        style = "margin: 0; color: #0c5460;")
    )
  })
}
