#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(treemap)
#library(d3treeR)
require(dplyr)
require(shiny)
require(gridBase)
require(RColorBrewer)
require(plotly)
require(leaflet)
ejecucionMes <- read.csv('EjecucionMensual.csv')
entidad <- read.csv('Entidad.csv', sep =  ';')
mapaJson <-  rgdal::readOGR(dsn ="guatemala.geojson")
#mapaJson <-  jsonlite::fromJSON("guatemala.geojson",layer = "guatemala")
### Handle cliks on a treemap
tmLocate <-
  function(coor, tmSave) {
    tm <- tmSave$tm
    
    # retrieve selected rectangle
    rectInd <- which(tm$x0 < coor[1] &
                       (tm$x0 + tm$w) > coor[1] &
                       tm$y0 < coor[2] &
                       (tm$y0 + tm$h) > coor[2])
    
    return(tm[rectInd[1], ])
    
  }
#######



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$condition <- renderText({
    condition()
  })
  
  output$tipoVisualizacion <- renderText({
    visualizacion()
  })
  
  condition <- reactive({
    refresh = detalleGasto()
    retornoVisualizacion = input$visualizacion
    print(retornoVisualizacion)
    input$visualizacion
    result = 0
    if( !is.null(refresh)){
      if(retornoVisualizacion != "Código.Departamento" ){
       if( refresh%%2 == 0 ){
          result = 0
         }else if(refresh%%2 == 1)
           result =1
      }
        else if( retornoVisualizacion == "Código.Departamento")
          if( refresh%%2 == 0 ){
            result = 0
          }else if( refresh%%2 == 1 )
           result = 2
    }
    print(paste("El resultado es: ", result))
    return(result)
  })
  
  
  visualizacion <- reactive({
    clasificacion = input$filtro
    result = 0
    if(clasificacion == "Código.Departamento"){
      result = 1
    }
    return(result)
  })
  
  
  outputOptions(output, 'condition', suspendWhenHidden=FALSE)
  
  porcentajeEjecucion <- reactive({
    ejecucionMes %>%
      select(Ejercicio, Devengado, Vigente ) %>%
      filter(Ejercicio == input$year ) %>%
      mutate(PorcentajeEje =  Devengado /   Vigente  * 100  ) 
  })
  
  
  
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Porcentaje de ejecución", paste( value = round(porcentajeEjecucion()$PorcentajeEje,1), "%" ) , icon = icon("percent"),
      color = "green", width = NULL
    )
  })
  
  
  
  output$devengadoBox <- renderInfoBox({
    infoBox(
      "Ejecución presupuestaria", paste("Q" , value = formatC( ejecucionMes[ejecucionMes$Ejercicio == input$year, ]$Devengado, format = "f", big.mark = ",", digits = 1) )  , icon = icon("money"),
      color = "green", width = NULL
    )
  })
  
  output$detalle <- renderUI({
    actionButton("detalleGasto", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
  })
  
  output$opcionesFiltro <- renderUI({
    radioButtons("filtro","Escoja la forma de ver los datos",
                 choices = c("Institución" = "Entidad",
                             "Finalidad",
                             "Clasificación geográfica" = "Código.Departamento",
                             "Objeto del gasto",
                             "Económico del gasto"
                 )
                 )
    #actionButton("", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
  })
  
  
  output$tipoVisualizacion <- renderUI({
    input$filtro
    vector = c()
    v = visualizacion()
    if(v  == '1'){
      vector = c("Treemap" = "Treemap",
                 "Tabla" = "tabla",
                 "Mapa departamental" = "Código.Departamento"
      )
    }else if(v == '0'){ 
      vector = c("Treemap" = "Treemap",
                 "Tabla" = "tabla")
    }
    radioButtons("visualizacion","Escoja la forma de ver los datos",
                 choices = vector
    )
    #actionButton("", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
  })
  
  
  detalleGasto=reactive({
    input$detalleGasto
  }) 
  
  
  gasto <- reactive({
    print( paste("La opción dada por el filtro es ",input$filtro )  )
    temp <- entidad %>%
      select(Ejercicio,Devengado, input$filtro)%>%
      filter(Ejercicio == input$year) %>%
      group_by_(input$filtro) %>%
      summarise(devengado = sum(Devengado))
    return(temp)
  })
  
  output$treemap1 <- renderPlot({
    # col = getRecordFromTreeMap()
    # print(col)
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    vps <- baseViewports()
    
    temp=gasto()
    .tm <-   treemap(temp, 
                    index=input$filtro, 
                    vSize="devengado", 
                    vColor="devengado",
                    type="value",
                    title = "",
                    palette="Blues",
                    border.col ="white",
                    position.legend="right",
                    fontsize.labels = 16,
                    title.legend="Escala de colores")
  })
  
  
  treemap_clicked <- reactiveValues(
    center = NULL,
    for_condition=NULL
  )
  
  
  # Handle clicks on treemap by country
  observeEvent(input$click_treemap, {
    x <- input$click_treemap$x
    y <- input$click_treemap$y
    treemap_clicked$center <- c(x,y)
    print(treemap_clicked$center)
    
    if(is.null(treemap_clicked$for_condition)){
      treemap_clicked$for_condition=c(x,y)
    }
    else{treemap_clicked$for_condition=NULL}
  })
  
  getRecordFromTreeMap <- reactive({
    x <- treemap_clicked$center[1]
    y <- treemap_clicked$center[2]
    
    x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
    y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
    
    
    l <- tmLocate(list(x=x, y=y), .tm)
    z=l[, 1:(ncol(l)-5)]
    
    
    if(is.na(z[,1]))
      return(NULL)
    
    col=as.character(z[,1])
    print(col)
    #filter(pop_data,Country==col)
  })
  
  output$mapa <- renderLeaflet({ 
  informacionMapa <- sp::merge(mapaJson, gasto(), by.x="Código.Departamento" )
  View(informacionMapa)
  print(gasto())
  qpal <- colorQuantile("YlGn", informacionMapa$devengado, n = 5, na.color = "#bdbdbd")


  lat <- 16
  lng <- -89.5
  zoom <- 7
  
      mapa <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
        addTiles()%>%
        setView(lat = lat, lng = lng, zoom = zoom)%>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = informacionMapa, fillColor = ~qpal(devengado), fillOpacity = 0.7,
                    color = "white", weight = 2)
    


  })
    
  
  
})
