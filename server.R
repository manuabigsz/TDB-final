library(shiny)
library(sf)
library(ggplot2)
library(raster)
library(DT)
library(leaflet)  
library(DBI)
library(RPostgres)

source("functions.R")

shinyServer(function(input, output, session) {
  
  sf_data <- load_balanco_hidrico()
  sf_data <- st_zm(sf_data, drop = TRUE, what = "ZM")
  
  output$mapPlot <- renderPlot({
    num_upg <- length(unique(sf_data$upg))
    
    fill_var <- if ((input$fill_var %in% c("upg", "id_balanco")) && num_upg > 50) {
      NULL
    } else {
      input$fill_var
    }
    
    ggplot(sf_data) +
      geom_sf(aes_string(fill = input$fill_var), color = "white", size = 0.2) +
      theme_minimal() +
      labs(
        title = "Unidades de Balanço Hídrico",
        fill = fill_var
      ) +
      theme(
        legend.position = if (input$fill_var == "upg" && num_upg > 50) "none" else "bottom",
        axis.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      )
  })
  
  observeEvent(input$plot_click, {
    tryCatch({
      click_point <- st_point(c(input$plot_click$x, input$plot_click$y))
      click_sf <- st_sfc(click_point, crs = st_crs(sf_data))
      
      click_sf <- st_zm(click_sf, drop = TRUE, what = "ZM")
      
      clicked_region <- st_intersects(click_sf, sf_data, sparse = FALSE)
      
      if (any(clicked_region)) {
        region_index <- which(clicked_region)[1]
        selected_data <- sf_data[region_index, ]
        
        bacia_info <- selected_data$bacia
        upg_info <- selected_data$upg
        id_balanco_info <- selected_data$id_balanco
        
        output$clickInfo <- renderUI({
          div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #007bff;",
            h4("Informações da Região Selecionada", style = "color: #007bff; margin-top: 0;"),
            p(strong("Bacia: "), bacia_info %||% "Não informado"),
            p(strong("UPG: "), upg_info %||% "Não informado"),
            p(strong("ID Balanço: "), id_balanco_info %||% "Não informado")
          )
        })
        
      } else {
        output$clickInfo <- renderUI({
          div(
            style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; border-left: 4px solid #ffc107;",
            p("Clique em uma região do mapa para ver as informações.", 
              style = "margin: 0; color: #856404;")
          )
        })
      }
      
    }, error = function(e) {
      output$clickInfo <- renderUI({
        div(
          style = "background-color: #f8d7da; padding: 15px; border-radius: 5px; border-left: 4px solid #dc3545;",
          h5("Erro ao processar clique", style = "color: #721c24; margin-top: 0;"),
          p(paste("Erro:", e$message), style = "margin: 0; color: #721c24; font-size: 12px;")
        )
      })
    })
  })
  
  output$clickInfo <- renderUI({
    div(
      style = "background-color: #d1ecf1; padding: 15px; border-radius: 5px; border-left: 4px solid #17a2b8;",
      p("Clique em uma região do mapa para ver as informações da Bacia e UPG.", 
        style = "margin: 0; color: #0c5460;")
    )
  })
  
  observe({
    raster_data <- load_raster_metadata()
    if(nrow(raster_data) > 0) {
      choices <- setNames(raster_data$id, 
                          paste0(raster_data$filename, " (", raster_data$date, ")"))
      updateSelectInput(session, "selected_raster", choices = choices)
    }
  })
  
  output$rasterTable <- DT::renderDataTable({
    df <- load_raster_metadata()
    
    if("bbox_wkt" %in% names(df)) {
      df$regiao <- sapply(df$bbox_wkt, function(bbox) {
        tryCatch({
          coords_match <- regmatches(bbox, regexpr("\\(\\(([^)]+)\\)\\)", bbox))
          if(length(coords_match) > 0) {
            coords_str <- gsub("\\(\\(|\\)\\)", "", coords_match)
            coords_pairs <- strsplit(coords_str, ",")[[1]]
            first_coord <- strsplit(trimws(coords_pairs[1]), " ")[[1]]
            lng <- as.numeric(first_coord[1])
            lat <- as.numeric(first_coord[2])
            
            # Verificar se está na região do Brasil
            is_brazil <- lng >= -74 && lng <= -34 && lat >= -34 && lat <= 5
            return(if(is_brazil) "Brasil" else "Fora do Brasil")
          }
          return("Desconhecida")
        }, error = function(e) "Erro")
      })
    }
    
    DT::datatable(df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle(
        "regiao",
        backgroundColor = DT::styleEqual(c("Brasil", "Fora do Brasil"), 
                                         c("#d4edda", "#f8d7da"))
      )
  })
  
  output$barPlot <- renderPlot({
    ggplot(sf_data) +
      geom_bar(aes(x = bacia, fill = bacia), show.legend = FALSE) +
      labs(title = "Quantidade de UPG por Bacia",
           x = "Bacia",
           y = "Quantidade") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$sfTable <- DT::renderDataTable({
    sf_df <- st_drop_geometry(sf_data)[, c("gid", "bacia", "upg", "id_balanco")]
    
    DT::datatable(sf_df, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  

  
  
  
  
  output$raster_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -51.1, lat = -28.2, zoom = 8) %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$raster_info <- renderText({
    if(is.null(input$selected_raster) || input$selected_raster == "") {
      return("Nenhum raster selecionado")
    }
    
    tryCatch({
      info <- get_raster_info(input$selected_raster)
      if(nrow(info) > 0) {
        paste(
          paste("ID:", info$id),
          paste("Data:", info$date),
          paste("Arquivo:", info$filename),
          paste("Largura:", info$width, "pixels"),
          paste("Altura:", info$height, "pixels"),
          paste("Número de bandas:", info$num_bands),
          paste("SRID:", info$srid),
          paste("Bounding Box:", info$bbox_wkt),
          sep = "\n"
        )
      } else {
        "Informações não disponíveis"
      }
    }, error = function(e) {
      paste("Erro ao carregar informações:", e$message)
    })
  })
  
  observeEvent(input$load_raster, {
    if(is.null(input$selected_raster) || input$selected_raster == "") {
      showNotification("Por favor, selecione um raster primeiro.", type = "warning")
      return()
    }
    
    tryCatch({
      showNotification("Carregando dados do raster...", type = "message")
      
      info <- get_raster_info(input$selected_raster)
      
      if(nrow(info) > 0) {
        raster_path <- file.path("C:/Users/manu/OneDrive/Documentos/tdb/raster/HYP_LR/HYP_LR/HYP_LR.tif")
        
        if(file.exists(raster_path)) {
          showNotification("Arquivo encontrado. Carregando raster...", type = "message")
          
          raster_data <- raster::raster(raster_path)
          
          current_crs <- NULL
          target_crs <- "+proj=longlat +datum=WGS84 +no_defs"
          
          tryCatch({
            current_crs <- as.character(raster_data@crs)
            if(is.na(current_crs) || is.null(current_crs) || current_crs == "" || current_crs == "NA") {
              
              crs(raster_data) <- CRS(target_crs)
              current_crs <- target_crs
              showNotification("CRS definido como WGS84 (padrão Natural Earth).", type = "message")
            } else if(!grepl("longlat", current_crs, ignore.case = TRUE)) {
              
              showNotification("Reprojetando raster para WGS84...", type = "message")
              raster_data <- projectRaster(raster_data, crs = CRS(target_crs))
              current_crs <- target_crs
              showNotification("Raster reprojetado para WGS84.", type = "message")
            }
          }, error = function(e) {
            showNotification(paste("Definindo CRS padrão devido a erro:", e$message), type = "warning")
            crs(raster_data) <- CRS(target_crs)
            current_crs <- target_crs
          })
          
          ext <- extent(raster_data)
          
          if(is.null(ext) || any(is.na(c(ext@xmin, ext@xmax, ext@ymin, ext@ymax)))) {
            stop("Extent do raster inválido")
          }
          
          raster_values <- getValues(raster_data)
          if(all(is.na(raster_values))) {
            stop("Raster não contém dados válidos")
          }
          
          center_lng <- mean(c(ext@xmin, ext@xmax), na.rm = TRUE)
          center_lat <- mean(c(ext@ymin, ext@ymax), na.rm = TRUE)
          
          if(is.na(center_lng) || is.na(center_lat)) {
            stop("Coordenadas do centro inválidas")
          }
          
          extent_width <- abs(ext@xmax - ext@xmin)
          extent_height <- abs(ext@ymax - ext@ymin)
          zoom_level <- if(extent_width > 10 || extent_height > 10) 4 else 
            if(extent_width > 1 || extent_height > 1) 6 else 8
          
          showNotification("Adicionando raster ao mapa...", type = "message")
          
          leafletProxy("raster_map") %>%
            clearShapes() %>%
            clearMarkers() %>%
            clearImages() %>%
            setView(lng = center_lng, lat = center_lat, zoom = zoom_level) %>%
           
            addRasterImage(raster_data, 
                           colors = colorNumeric(
                             palette = c("#543005", "#8c510a", "#bf812d", "#dfc27d", 
                                         "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", 
                                         "#01665e", "#003c30"), 
                             domain = NULL, na.color = "transparent"
                           ),
                           opacity = 0.7,
                           project = TRUE,
                           group = "Natural Earth Raster") %>%
           
            addRectangles(
              lng1 = ext@xmin, lat1 = ext@ymin,
              lng2 = ext@xmax, lat2 = ext@ymax,
              fillColor = "transparent",
              color = "#2c3e50",
              weight = 1,
              opacity = 0.8,
              popup = paste0(
                "<strong>Natural Earth Raster</strong><br/>",
                "Arquivo: HYP_LR.tif<br/>",
                "Tipo: Tons Hipsométricos<br/>",
                "Dimensões: ", ncol(raster_data), " x ", nrow(raster_data), " pixels<br/>",
                "Resolução: ~", round(res(raster_data)[1], 4), "° x ", round(res(raster_data)[2], 4), "°<br/>",
                "Extent: [", round(ext@xmin, 2), ", ", round(ext@ymin, 2), 
                "] - [", round(ext@xmax, 2), ", ", round(ext@ymax, 2), "]<br/>",
                "Centro: [", round(center_lng, 2), ", ", round(center_lat, 2), "]"
              )
            ) %>%
            
            addLayersControl(
              baseGroups = c("OpenStreetMap", "Satellite"),
              overlayGroups = c("Natural Earth Raster"),
              options = layersControlOptions(collapsed = FALSE)
            ) %>%
            
            addLegend(
              position = "bottomright",
              title = "Elevação/Relevo<br/><small>(Natural Earth)</small>",
              colors = c("#543005", "#8c510a", "#bf812d", "#dfc27d", 
                         "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", 
                         "#01665e", "#003c30"),
              labels = c("Mais baixo", "", "", "", "", "", "", "", "", "Mais alto"),
              opacity = 0.7
            )
          
          showNotification("Raster carregado com sucesso no mapa!", type = "message")
          
        } else {
          
          showNotification("Arquivo raster não encontrado. Mostrando apenas bounding box.", type = "warning")
          
          
          bbox_wkt <- info$bbox_wkt
          coords_match <- regmatches(bbox_wkt, regexpr("\\(\\(([^)]+)\\)\\)", bbox_wkt))
          
          if(length(coords_match) > 0) {
            coords_str <- gsub("\\(\\(|\\)\\)", "", coords_match)
            coords_pairs <- strsplit(coords_str, ",")[[1]]
            
            coords_list <- lapply(coords_pairs, function(pair) {
              coord <- strsplit(trimws(pair), " ")[[1]]
              list(lng = as.numeric(coord[1]), lat = as.numeric(coord[2]))
            })
            
            lngs <- sapply(coords_list, function(x) x$lng)
            lats <- sapply(coords_list, function(x) x$lat)
            center_lng <- mean(lngs, na.rm = TRUE)
            center_lat <- mean(lats, na.rm = TRUE)
            
            leafletProxy("raster_map") %>%
              clearShapes() %>%
              clearMarkers() %>%
              setView(lng = center_lng, lat = center_lat, zoom = 8) %>%
              addPolygons(
                lng = lngs,
                lat = lats,
                color = "#ff0000",
                weight = 2,
                opacity = 1,
                fillColor = "#ff0000",
                fillOpacity = 0.1,
                popup = paste0(
                  "<strong>Bounding Box do Raster:</strong><br/>",
                  "Arquivo: ", info$filename, "<br/>",
                  "AVISO: Arquivo não encontrado em: ", raster_path
                )
              )
          }
        }
        
      } else {
        showNotification("Erro: Raster não encontrado no banco de dados.", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("Erro ao carregar raster:", e$message), type = "error")
    })
  })
  
})

