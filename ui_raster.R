library(leaflet)
library(DT)

raster_tab <- tabPanel("Raster",
                       fluidRow(
                         column(12,
                                div(
                                  style = "padding: 20px;",
                                  
                                  div(
                                    style = "margin-bottom: 30px;",
                                    h4("Visualização de Dados Raster", style = "color: #2c3e50; margin-bottom: 20px;"),
                                    
                                    fluidRow(
                                      column(6,
                                             selectInput("selected_raster", 
                                                         "Selecione um arquivo raster:",
                                                         choices = NULL,
                                                         selected = NULL)
                                      ),
                                      column(6,
                                             div(style = "margin-top: 25px;",
                                                 actionButton("load_raster", "Carregar no Mapa", 
                                                              class = "btn-primary",
                                                              style = "width: 100%; height: 40px;")
                                             )
                                      )
                                    )
                                  ),
                                  
                                  div(
                                    style = "margin-bottom: 30px;",
                                    h5("Mapa Interativo", style = "color: #2c3e50; margin-bottom: 15px;"),
                                    leafletOutput("raster_map", height = "500px")
                                  ),
                                  
                                  div(
                                    style = "margin-bottom: 30px;",
                                    conditionalPanel(
                                      condition = "input.selected_raster != null && input.selected_raster != ''",
                                      div(
                                        style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; border-left: 4px solid #007bff;",
                                        h5("Informações do Raster", style = "color: #2c3e50; margin-top: 0;"),
                                        verbatimTextOutput("raster_info")
                                      )
                                    )
                                  ),
                                  
                                  div(
                                    style = "margin-top: 30px;",
                                    h5("Arquivos Raster Disponíveis", style = "margin-bottom: 20px; color: #2c3e50;"),
                                    DT::dataTableOutput("rasterTable")
                                  ),
                                  
                                  div(
                                    style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                   color: white; padding: 30px; border-radius: 15px; margin: 30px 0;",
                                    
                                    h4("Natural Earth Data", style = "margin-top: 0;"),
                                    h5("Detalhes da Fonte:", style = "color: #f1f1f1;"),
                                    
                                    tags$ul(
                                      style = "font-size: 15px; line-height: 1.5; color: #f1f1f1;",
                                      tags$li(HTML("<strong>Fonte:</strong> Natural Earth Data")),
                                      tags$li(HTML("<strong>URL:</strong> <a href='https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/HYP_LR.zip' target='_blank' style='color: #d1ecf1;'>Clique aqui para acessar</a>")),
                                      tags$li(HTML("<strong>Formato:</strong> Raster (tons hipsométricos, baixa resolução)")),
                                      tags$li(HTML("<strong>Descrição:</strong> O conjunto de dados raster apresenta tons hipsométricos, representando variações altitudinais e características topográficas."))
                                    )
                                  )
                                )
                         )
                       )
)