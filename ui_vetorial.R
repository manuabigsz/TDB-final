vetorial_tab <- tabPanel("Vetorial",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("fill_var", 
                                         "Colorir mapa por:", 
                                         choices = c("Bacia" = "bacia", 
                                                     "UPG" = "upg", 
                                                     "ID Balanço" = "id_balanco"), 
                                         selected = "bacia"),
                             uiOutput("upgSelectUI"), 
                             hr(),
                             h4("Informações da Região"),
                             uiOutput("clickInfo")
                             
                           
                           ),
                           
                           
                           mainPanel(
                             h3("Visualização das Unidades de Balanço Hídrico"),
                             tabsetPanel(
                               tabPanel("Mapa Vetorial", 
                                        leafletOutput("mapPlot", height = "700px")),
                               
                               tabPanel("Gráfico", 
                                        plotOutput("barPlot", height = "400px")),
                               
                               tabPanel("Tabela", 
                                        DT::dataTableOutput("sfTable"))
                             )
                           )
                           
                           
                         ),
                        
                       
                         div(
                           style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
           color: white; padding: 30px; border-radius: 15px; margin: 30px auto; max-width: 100%;",
                           
                           h4("Balanço Hídrico RS", style = "margin-top: 0;"),
                           
                           
                           h5("Sobre a base de dados vetorial utilizada:", style = "color: #f1f1f1;"),
                           
                           tags$ul(
                             style = "font-size: 15px; line-height: 1.5; color: #f1f1f1;",
                             tags$li(HTML("<strong>Fonte:</strong> Secretaria do Meio Ambiente e Infraestrutura do Estado do Rio Grande do Sul (SEMA-RS)")),
                             tags$li(HTML("<strong>URL:</strong> <a href='https://www.sema.rs.gov.br/upload/arquivos/202406/24164843-unidades-balanco-hidrico.zip' target='_blank' style='color: #d1ecf1;'>Clique aqui para acessar</a>")),
                             tags$li(HTML("<strong>Formato:</strong> Shapefile (.shp e arquivos associados)")),
                             tags$li(HTML("<strong>Descrição:</strong> O conjunto de dados contém informações vetoriais georreferenciadas referentes às unidades de balanço hídrico do estado do Rio Grande do Sul. Os shapefiles representam feições hídricas (como bacias hidrográficas e corpos d’água) por meio de geometrias como polígonos, linhas ou pontos."))
                           )
                         )
                         
)
