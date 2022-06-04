library(shiny)
library(shinythemes)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(ggrepel)
library(shinyWidgets)
library(enrichR)
library(markdown)


# setwd('/media/jmartinezv/DATOS/TF_NETWORK/Fran_subnetworks/Shiny_app/')

# Data
# Homeostatic <- read.csv("./Networks/Homeostatic_Merged_Network_TF_TG_default_edge_datastudio.csv")
# GATA4KO <- read.csv("./Networks/Difference_NT_Gata4KO-NT_p48Cre_default_edge_datastudio_2.csv")
# GATA6KO <- read.csv("./Networks/Difference_NT_Gata6KO-NT_p48Cre_default_edge_datastudio.csv")
# KRAS <- read.csv("./Networks/Difference_PBS_Kras_NT_p48Cre_default_edge_datastudio.csv")
# GATA4KO_KRAS_context <- read.csv("./Networks/Difference_PBS_Kras_Gata4KO-PBS_Kras_default_edge_datastudio_2.csv")
# CAE_KRAS_context <- read.csv("./Networks/Difference_Cae_Kras_PBS_Kras_default_edge_datastudio.csv")
# GATA4KO_CAE_KRAS_context <- read.csv("./Networks/Difference_Cae_Kras_Gata4KO-Cae_Kras_default_edge_datastudio_2.csv")
# CAE_GATA4KO_KRAS_context <- read.csv("./Networks/Difference_Cae_Kras_Gata4KO-PBS_Kras_Gata4KO_default_edge_datastudio_2.csv")

Homeostatic <- read.csv("./data/edges/Homeostatic_Merged_Network_TF_TG_default_edge_datastudio.csv")
GATA4KO <- read.csv("./data/edges/Difference_NT_Gata4KO-NT_p48Cre_default_edge_datastudio.csv")
GATA6KO <- read.csv("./data/edges/Difference_NT_Gata6KO-NT_p48Cre_default_edge_datastudio.csv")
KRAS <- read.csv("./data/edges/Difference_PBS_Kras_NT_p48Cre_default_edge_datastudio.csv")
GATA4KO_KRAS_context <- read.csv("./data/edges/Difference_PBS_Kras_Gata4KO-PBS_Kras_default_edge_datastudio.csv")
CAE_KRAS_context <- read.csv("./data/edges/Difference_Cae_Kras_PBS_Kras_default_edge_datastudio.csv")
GATA4KO_CAE_KRAS_context <- read.csv("./data/edges/Difference_Cae_Kras_Gata4KO-Cae_Kras_default_edge_datastudio.csv")
CAE_GATA4KO_KRAS_context <- read.csv("./data/edges/Difference_Cae_Kras_Gata4KO-PBS_Kras_Gata4KO_default_edge_datastudio.csv")


# Lista con las variables que contienen a los datos. Se les asigna una etiqueta a cada conjunto de datos (Network1, Network2, etc)
networks <- list(Homeostatic = Homeostatic,
                 GATA4KO = GATA4KO, 
                 GATA6KO = GATA6KO,
                 KRAS = KRAS,
                 GATA4KO_KRAS_context = GATA4KO_KRAS_context,
                 CAE_KRAS_context = CAE_KRAS_context,
                 GATA4KO_CAE_KRAS_context = GATA4KO_CAE_KRAS_context,
                 CAE_GATA4KO_KRAS_context = CAE_GATA4KO_KRAS_context
                 )

# Data degrees - Redes de base
# NT_P48CRE_degree <- read.csv("./Networks/NT_p48Cre_degree.csv")
# PBS_KRAS_degree <- read.csv("./Networks/PBS_Kras_degree.csv")
# CAE_KRAS_GATA4KO_degree <- read.csv("./Networks/Cae_Kras_Gata4KO_degree.csv")
# PBS_KRAS_GATA4KO_degree <- read.csv("./Networks/PBS_Kras_Gata4KO_degree.csv")

NT_P48CRE_degree <- read.csv("./data/degrees/NT_p48Cre_degree.csv")
PBS_KRAS_degree <- read.csv("./data/degrees/PBS_Kras_degree.csv")
CAE_KRAS_GATA4KO_degree <- read.csv("./data/degrees/Cae_Kras_Gata4KO_degree.csv")
PBS_KRAS_GATA4KO_degree <- read.csv("./data/degrees/PBS_Kras_Gata4KO_degree.csv")

# Lista de bases de datos para el analisis funcional
databases <- c("WikiPathways_2019_Mouse",
               "KEGG_2019_Mouse",
               "MSigDB_Hallmark_2020",
               "GO_Biological_Process_2021",
               "GO_Cellular_Component_2021",
               "GO_Molecular_Function_2021",
               "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X",
               "ChEA_2016",
               "RNAseq_Automatic_GEO_Signatures_Mouse_Down",
               "RNAseq_Automatic_GEO_Signatures_Mouse_Up"
               )

#################################
# # # # # # # UI # # # # # # # #
#################################

# Define UI ----
ui <- fluidPage(
  #theme = shinytheme("slate"), #shinythemes::themeSelector(),
  #titlePanel("ATACseq footprint network analyzer"),
  #tags$style(HTML(".js-irs-0 .irs-bar {background: purple; border-color: red}")),
  #tags$style(HTML(".js-irs-1 .irs-bar {background: purple; border-color: red}")),
  tags$style(HTML('
         #helpButton {
         margin-top: 30px;
         margin-bottom: 30px;
         }
         
         #noData {
         margin-top: 210px;
         /*display: flex;*/
         /*align-items: center;*/
         /*justify-content: center;*/
         }
  
         #fluidRow3 {
         height: 200px;
         margin-top: 30px;
         margin-bottom: 40px;
         }
         
         #sliders {
         margin-top: 0px;
         }
         
         .dataTables_filter {
         float: right !important;
         }
  ')
  ),
  
  ########################
  # ROWS # 
  ########################
  
  #################################
  # Fila 1: Titulo y boton de ayuda
  #################################
  fluidRow(
    column(9,
           titlePanel("ATACseq footprint network analyzer")
           ),
    column(3,
           id = "helpButton",
           actionButton("help", 
                        "Help", 
                        icon = icon("info-circle")
                        ),
           align = "right"
           )
    ),
  
  ##################################################
  # Fila 2: Seleccion del dataset y tablas de degree
  ##################################################
  fluidRow(
    column(5,
           selectInput("net",
                       "Network:",
                       choices = c(names(networks)),
                       selected = "Homeostatic"),
           uiOutput(outputId = "diagVenn")
    ),
    column(3,
           h4(tagAppendAttributes(textOutput("titleDegree2"), style="white-space:pre-wrap;")),
           conditionalPanel(
             id = "noData",
             condition = "input.net == 'Homeostatic'",
             helpText("(No table is shown here as it is not necessary since for 
                      the HOMEOSTATIC data no comparison has been made)")),
           conditionalPanel(
             condition = "input.net != 'Homeostatic'",
             DT::dataTableOutput("tdegree2"))
    ),       
    column(3,
           h4(tagAppendAttributes(textOutput("titleDegree"), style="white-space:pre-wrap;")),
           DT::dataTableOutput("tdegree"))
  ),

  #####################################
  # Fila 3: SelectInputs y sliderInputs
  #####################################
  fluidRow(id = "fluidRow3",
           column(3,
                  align="center",
                  selectInput("fp",
                              "Footprint:",
                              c("All",
                                unique(Homeostatic$Footprint))),
           uiOutput(outputId = "seqLogo")
    ),
    column(3,
           align="center",
           selectInput("loc",
                       "Footprint location:",
                       c("All",
                         as.character(unique(Homeostatic$Location)))),
             plotOutput("locPie", height = 120)
    ),
    column(3,
           align="center",
           id = "sliders",
           sliderInput("tfbs",
                       "TFBS Score:",
                       min = min(Homeostatic$TFBS_score),
                       max = max(Homeostatic$TFBS_score),
                       value = c(min(Homeostatic$TFBS_score),max(Homeostatic$TFBS_score))
                       ),
           plotOutput("dpTFBS", height = 120)
    ),
    column(3,
           align="center",
           id = "sliders",
           sliderInput("footsc",
                       "Footprint Score:",
                       min = min(Homeostatic$Footprint_score),
                       max = max(Homeostatic$Footprint_score),
                       value = c(min(Homeostatic$Footprint_score),max(Homeostatic$Footprint_score))
                       ),
           plotOutput("dpFoot", height = 120)
    )
  ),
  
  # setBackgroundImage(
  #     src = "/background/cef5880535106bc390e54f3791ac5b57.jpg"),

  #######################
  # Fila 4: Tabla general
  #######################
  DT::dataTableOutput("table"),

  # Boton para descargar los datos
  downloadButton('dload',"Download table"),

  ##################################################
  # Fila 5: Lista desplegable del analisis funcional
  ##################################################
  # Titulo Functional Analysis
  h3("Functional Analysis"),
  fluidRow(
    column(4,
          selectInput("db",
                       "Query database:",
                      c("WikiPathways_2019_Mouse",
                         databases))
          ),
    column(3,
           actionButton("faButton","Run functional analysis"))
    ),
  # Plot del analisis funcional
  fluidRow(
    column(6,
           conditionalPanel(
             condition = "input.fp == 'All'",
             helpText("Please, select a footprint to generate the functional analysis.")),
           conditionalPanel(
             condition = "input.fp != 'All'",
             plotOutput("faPlot"))
           )
    ),

  ######################################
  # Fila 6: Tabla del analisis funcional
  ######################################
  fluidRow(
    column(12,
           conditionalPanel(
             condition = "input.fp == 'All'",
             helpText =""),
           conditionalPanel(
             condition = "input.fp != 'All'",
             DT::dataTableOutput("faTable"))
    )
  )
)

####################################
# # # # # # # SERVER # # # # # # # #
####################################

# Define server logic ----
server <- function(input, output, session) {
  
  # Boton de HELP
  observeEvent(input$help, {
    showModal(modalDialog(
      title = NULL,
      footer = NULL,
      size = c("l"),
      includeMarkdown("./data/help/help.md"),
      easyClose = TRUE
    ))
  })
  
  # Devuelve el dataset correcto
  dataset <- Homeostatic
  datasetInput <- reactive({
    if (input$net != "Homeostatic"){
      dataset <- networks[[as.character(input$net)]]
    }
    if (input$fp != "All") {
      dataset <- dataset[dataset$Footprint == input$fp,]
    }
    if (input$loc != "All") {
      dataset <- dataset[dataset$Location == input$loc,]
    }
    if(input$tfbs[1] != min(dataset$TFBS_score) | input$tfbs[2] != max(dataset$TFBS_score)){
      dataset <- dataset[dataset$TFBS_score > input$tfbs[1] & dataset$TFBS_score < input$tfbs[2],]
    }
    if(input$footsc[1] != min(dataset$Footprint_score) | input$footsc[2] != max(dataset$Footprint_score)){
      dataset <- dataset[dataset$Footprint_score > input$footsc[1] & dataset$Footprint_score < input$footsc[2],]
    }
    return(dataset)
  })
  
  # Devuelve la tabla con los datos de degree
  degreeInput <- reactive({
    dataset <- networks[[as.character(input$net)]]
    tableDegree <- data.frame(table(dataset$Footprint))
    tableDegree <- tableDegree[order(-tableDegree$Freq),]
    names(tableDegree) <- c("Footprint","Degree")
    return(tableDegree)
  })
  
  # Devuelve la tabla con los datos de degree de las redes de base
  degreeInput2 <- reactive({
    tableDegree2 <- switch(input$net,
                           "GATA4KO" = list(NT_P48CRE_degree, "NT_P48CRE"),
                           "GATA6KO" = list(NT_P48CRE_degree, "NT_P48CRE"),
                           "KRAS" = list(NT_P48CRE_degree, "NT_P48CRE"),
                           "GATA4KO_KRAS_context" = list(PBS_KRAS_degree, "PBS_KRAS*"),
                           "CAE_KRAS_context" = list(PBS_KRAS_degree, "PBS_KRAS*"),
                           "GATA4KO_CAE_KRAS_context" = list(CAE_KRAS_GATA4KO_degree, "CAE_KRAS*"),
                           "CAE_GATA4KO_KRAS_context" = list(PBS_KRAS_GATA4KO_degree, "PBS_KRAS*_GATA4KO"))
    return(tableDegree2)
  })
  
  # Devuelve los resultados de enrichR
  getAF <- eventReactive(input$faButton, {
    websiteLive <- TRUE
    dbs <- listEnrichrDbs()
    if (is.null(dbs)){
      websiteLive <- FALSE
    }
    if (websiteLive) {
      enriched <- enrichr(datasetInput()$Target, input$db)
    }
    return(enriched)
  })
  
  # Actualizacion dinamica de los drop-down:
  # Utiliza la funcion oberve() para detectar los cambios en el drop down de referencia (el de las redes)
  # Cuando se produce un cambio en ese drop down (que aqui se referencia como input$net) se actualiza el drop down indicado:
  # fp, tgbs o footsc
  observe({
    updateSelectInput(session, 
                      "fp", 
                      choices = c("All", unique(networks[[input$net]]$Footprint)), 
                      selected = "All")
    updateSliderInput(session, 
                      "tfbs", 
                      min = min(networks[[input$net]]$TFBS_score),
                      max = max(networks[[input$net]]$TFBS_score),
                      value = c(min(networks[[input$net]]$TFBS_score), max(networks[[input$net]]$TFBS_score)))
                      # min = min(datasetInput()$TFBS_score),
                      # max = max(datasetInput()$TFBS_score),
                      # value = c(min(datasetInput()$TFBS_score), max(datasetInput()$TFBS_score)))
    updateSliderInput(session, 
                      "footsc", 
                      min = min(networks[[input$net]]$Footprint_score),
                      max = max(networks[[input$net]]$Footprint_score),
                      value = c(min(networks[[input$net]]$Footprint_score), max(networks[[input$net]]$Footprint_score)))
                      # min = min(datasetInput()$Footprint_score), 
                      # max = max(datasetInput()$Footprint_score), 
                      # value = c(min(datasetInput()$Footprint_score), max(datasetInput()$Footprint_score)))
  })
  
  ## OUTPUTS ##
  
  # Output de la tabla general - Variable: table
  # Se anade style = "boostrap" para que la tabla use el mismo tema que el
  # resto de elementos
  # Se anade options...regex = TRUE para poder usar expresiones regulares
  # en el buscador
  output$table <- DT::renderDataTable (
    datasetInput(), 
    options = list(search = list(regex = TRUE), 
                   language = list(search = "<i class='glyphicon glyphicon-search'></i>")), 
    style = "bootstrap"
  )
  
  # Output para la descarga de los datos de la tabla general - Variable: dload
  # Para poder descargar los resultados de una tabla se ha anadido [input[["table_rows_all"]], ]
  output$dload <- downloadHandler(
    filename = function(){
      paste("TableNetworkAnalysis-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(datasetInput()[input[["table_rows_all"]], ], file)
    }
  )
  
  # Output de la tabla de degree - Variable: tdegree
  output$tdegree <- DT::renderDataTable (
    degreeInput(), style = "bootstrap",
    options = list(bPaginate = FALSE, scrollY = "300px", 
                   language = list(search = "<i class='glyphicon glyphicon-search'></i>")),
    rownames= FALSE
  )
  
  # Output de la tabla de degree de las redes de base - Variable: tdegree2
  output$tdegree2 <- DT::renderDataTable (
    degreeInput2()[[1]], style = "bootstrap",
    options = list(bPaginate = FALSE, scrollY = "300px",
                   language = list(search = "<i class='glyphicon glyphicon-search'></i>")),
    rownames= FALSE
  )
  
  # Output de los titulos de las tablas de degree - Variable: titleDegree
  output$titleDegree <- renderText({
    paste(input$net, "TFs ranked by degree", sep = "\n")
  })
  
  # Output de los titulos de las tablas de degree - Variable: titleDegree2
  output$titleDegree2 <- renderText({
    ifelse(input$net == 'Homeostatic',"No data", paste(degreeInput2()[[2]], "TFs ranked by degree", sep = "\n"))
  })
  
  # Output de la imagen de los diagramas de Venn - Variable: diagVenn
  output$diagVenn <- renderUI({
      img(src=paste("venns/",input$net,"_edges_Venn.png",sep=""), height = 300)
  })
  
  # Output de la imagen de las secuencias logo - Variable: seqLogo
  output$seqLogo <- renderUI({
    if(input$fp != "All"){
      imgPath <- list.files(path = "www/seq_logo", pattern = paste(input$fp, "_", sep=""), 
                          ignore.case = TRUE)
      img(src = paste("seq_logo/", imgPath, sep=""), height = 80)
    }
  })
  
  # Output del pie chart - Variable: locPie
  output$locPie <- renderPlot({
    tableLoc <- data.frame(table(datasetInput()$Location))
    names(tableLoc)[1] <- "LocationName"
    perc <- round(tableLoc$Freq/sum(tableLoc$Freq)*100)
    labelsLoc <- paste(perc, "%", sep = "")
    # ifelse(nrow(datasetInput()) != 0,
    #        "No data",
           ggplot(tableLoc, aes(x = "", y = perc, fill = LocationName)) +
             geom_col(color = "black") +
             geom_label_repel(aes(label = labelsLoc), color = "black",
                              position = position_stack(vjust = 0.5),
                              show.legend = FALSE) +
             guides(fill = guide_legend(title = "Location")) +
             scale_fill_viridis_d() +
             coord_polar(theta = "y") +
             theme_void()
           # )
  }, res = 72) #res -> resolution
  
  # Output del density plot del TFBS score - Variable: dpTFBS
  output$dpTFBS <- renderPlot({
    if(input$fp != "All"){
      TFBSs <- datasetInput()["TFBS_score"] #Para obtener una sola columna del DF con formato de DF
      ggplot(TFBSs, aes(x=TFBS_score)) + geom_density(color = "darkblue", fill = "lightblue")
    }
  })
  
  # Output del density plot del Footprint score - Variable: dpFoot
  output$dpFoot <- renderPlot({
    if(input$fp != "All"){
      FPs <- datasetInput()["Footprint_score"] #Para obtener una sola columna del DF con formato de DF
      ggplot(FPs, aes(x=Footprint_score)) + geom_density(color = "darkblue", fill = "lightblue")
    }
  })
  
  # Output del grafico del analisis funcional - Variable: faPlot
  output$faPlot <- renderPlot({
    plotEnrich(getAF()[[1]], showTerms = 20, numChar = 40, y = "Count", orderBy = "P.value")
  })
  
  # Output de la tabla del analisis funcional - Variable: afTable
  output$faTable <- DT::renderDataTable(
    getAF()[[1]],
    options = list(search = list(regex = TRUE), 
                   language = list(search = "<i class='glyphicon glyphicon-search'></i>")),
    style = "bootstrap",
    caption = "Table Enrichment Analysis"
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
