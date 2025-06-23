library(shiny)
library(leaflet)
library(DT)
library(plotly)

raster_tab <- tabPanel(
  "Raster",
  
  fluidPage(
    h3("Visualização de Dados Raster", style = "margin-top: 10px; color: #2c3e50;"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("raster_choice", "Selecione um arquivo raster:", choices = NULL),
        actionButton("load_raster", "Carregar no Mapa", class = "btn btn-primary", width = "100%"),
        br(), br(),
        
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #667eea;",
          h5("Informações Técnicas", style = "color: #2c3e50; margin-top: 0;"),
          verbatimTextOutput("raster_tech_info", placeholder = TRUE)
        ),
        
        br(),
        h5("Detalhes do Raster"),
        verbatimTextOutput("raster_info")
      ),
      
      mainPanel(
        width = 9,
        
        div(
          style = "margin-bottom: 20px;",
          leafletOutput("raster_map", height = "400px")
        ),
        
        tabsetPanel(
          type = "tabs",
          
          tabPanel(
            "Distribuição de Valores",
            br(),
            plotlyOutput("raster_stats_plot", height = "350px")
          ),
          
          tabPanel(
            "Estatísticas Resumidas", 
            br(),
            plotlyOutput("raster_summary_plot", height = "350px")
          ),
          
          tabPanel(
            "Tabela de Arquivos",
            br(),
            DTOutput("rasterTable")
          )
        ),
        
        br(),
        
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