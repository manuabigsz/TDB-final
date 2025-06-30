library(leaflet)
rasterServer <- function(input, output, session) {
  
  observe({
    raster_files <- load_raster_files()
    updateSelectInput(session, "raster_choice",
                      choices = setNames(raster_files$id, raster_files$filename))
    
    output$rasterTable <- DT::renderDT({
      DT::datatable(raster_files, rownames = FALSE)
    })
  })
  
  raster_data <- reactiveVal(NULL)
  
  observeEvent(input$load_raster, {
    req(input$raster_choice)
    r <- load_raster_by_id(input$raster_choice)
    raster_data(r)
    
    output$raster_info <- renderPrint({
      r
    })
  })
  
  output$raster_stats_plot <- renderPlotly({
    req(raster_data())
    r <- raster_data()
    
    values_vec <- values(r)
    values_clean <- values_vec[!is.na(values_vec)]
    
    if(length(values_clean) == 0) {
      return(NULL)
    }
    
    hist_data <- data.frame(valores = values_clean)
    
    p <- ggplot(hist_data, aes(x = valores)) +
      geom_histogram(bins = 50, fill = "#667eea", alpha = 0.7, color = "white") +
      labs(
        title = "Distribuição dos Valores do Raster",
        x = "Valores",
        y = "Frequência"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, color = "#2c3e50"),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank()
      )
    
    plotly::ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$raster_summary_plot <- renderPlotly({
    req(raster_data())
    r <- raster_data()
    
    values_vec <- values(r)
    values_clean <- values_vec[!is.na(values_vec)]
    
    if(length(values_clean) == 0) {
      return(NULL)
    }
    
    stats <- data.frame(
      Estatística = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo"),
      Valor = c(
        min(values_clean, na.rm = TRUE),
        quantile(values_clean, 0.25, na.rm = TRUE),
        median(values_clean, na.rm = TRUE),
        mean(values_clean, na.rm = TRUE),
        quantile(values_clean, 0.75, na.rm = TRUE),
        max(values_clean, na.rm = TRUE)
      )
    )
    
    p <- ggplot(stats, aes(x = reorder(Estatística, Valor), y = Valor)) +
      geom_col(fill = "#764ba2", alpha = 0.8) +
      geom_text(aes(label = round(Valor, 2)), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = "Estatísticas Resumidas do Raster",
        x = "Estatísticas",
        y = "Valores"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, color = "#2c3e50"),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank()
      )
    
    plotly::ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$raster_tech_info <- renderText({
    req(raster_data())
    r <- raster_data()
    
    values_vec <- values(r)
    values_clean <- values_vec[!is.na(values_vec)]
    
    paste(
      paste("Dimensões:", nrow(r), "x", ncol(r), "pixels"),
      paste("Resolução:", paste(res(r), collapse = " x ")),
      paste("Sistema de Coordenadas:", crs(r)),
      paste("Total de Células:", ncell(r)),
      paste("Células Válidas:", length(values_clean)),
      paste("Células NA:", sum(is.na(values_vec))),
      paste("Faixa de Valores:", round(min(values_clean, na.rm = TRUE), 2), "a", round(max(values_clean, na.rm = TRUE), 2)),
      sep = "\n"
    )
  })
  
  output$raster_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -60, lat = -15, zoom = 4)
  })
  
  observe({
    req(raster_data())
    r <- raster_data()
    
    pal <- colorNumeric("viridis", domain = values(r), na.color = "transparent")
    
    leafletProxy("raster_map") %>%
      clearImages() %>%
      addRasterImage(r, colors = pal, opacity = 0.7, project = FALSE) %>%
      fitBounds(lng1 = extent(r)[1], lat1 = extent(r)[3],
                lng2 = extent(r)[2], lat2 = extent(r)[4]) %>%
      addLegend(pal = pal, values = values(r), title = "Valor do Raster")
  })
  
}
