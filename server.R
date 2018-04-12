#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(treemap)
require(dplyr)
require(shiny)
require(gridBase)
require(RColorBrewer)
require(plotly)
require(leaflet)
require(flifo)
require(lazyeval)


ejecucionMes <- read.csv('EjecucionMensual.csv')
entidad <- read.csv('Sabana_de_prueba_TF.csv', sep =  ';')
datos_intitucional <- read.csv('Jerarquia_Entidad.csv', sep=";")
datos_finalidad <- read.csv('Jerarquia_Finalidad.csv', sep= ';')
datos_geografico <- read.csv('Jerarquia_Geografico.csv', sep=';')
datos_objeto_gasto <- read.csv('Jerarquia_ObjetoGasto.csv', sep=';')
datos_economico <- read.csv('Jerarquia_Economico.csv', sep=';')
mapaJson <-  NULL
datos_tabla <- NULL
tabla_temporal <- NULL
tabla_dinamica <- NULL


# Setting up the environmet for global variables --------------------------
app.env <- new.env()

app.env$nivelActivo <- ""

jerarquia_institucional_ida = lifo()
push(jerarquia_institucional_ida, "Unidad.Ejecutora" )
push(jerarquia_institucional_ida, "Entidad" )

jerarquia_institucional_regreso = lifo()

jerarquia_finalidad_ida = lifo()
push(jerarquia_finalidad_ida, "División")
push(jerarquia_finalidad_ida, "Función")
push(jerarquia_finalidad_ida, "Finalidad")


jerarquia_finalidad_regreso = lifo()

jerarquia_geografico_ida = lifo()
push(jerarquia_geografico_ida, "Municipio")
push(jerarquia_geografico_ida, "Departamento")
push(jerarquia_geografico_ida, "Región")

jerarquia_geografico_regreso = lifo()

jerarquia_objeto_gasto_ida = lifo()
push(jerarquia_objeto_gasto_ida, "Renglón")
push(jerarquia_objeto_gasto_ida, "Sub.Grupo.Gasto")
push(jerarquia_objeto_gasto_ida, "Grupo.Gasto")

jerarquia_objeto_gasto_regreso = lifo()

jerarquia_economico_ida = lifo()
push(jerarquia_economico_ida, "Clasificación.Económica.Gasto")
push(jerarquia_economico_ida, "Económico.Nivel.Operativo")
push(jerarquia_economico_ida, "Económico.Nivel.4")
push(jerarquia_economico_ida, "Económico.Nivel.3")
push(jerarquia_economico_ida, "Económico.Nivel.2")
#push(jerarquia_economico_ida, "Económico.Nivel.1")


jerarquia_economico_regreso = lifo()


dimension_ida = NULL # candidato a ser eliminado
valor_dimension = NULL #candidato a ser eliminado

jerarquia_dimension_regreso = list()  #cambio de estructura de datos
jerarquia_valor_dimension_regreso = list() #antes era lifo, se pasa a lista



opciones_filtro_inicio <- list("Subgrupo Institucional","Finalidad","Región","Grupo",
  "Económico del gasto" = "Económico.Nivel.1"
)

mascara_filtro_inicio <- list("Sub.Grupo",
                           "Finalidad",
                           "Clasificación geográfica",
                           "Objeto del gasto" ,
                           "Económico del gasto" 
)
### Handle cliks on a treemap
tmLocate <- function(coor, tmSave) {
    tm <- tmSave$tm
    
    # retrieve selected rectangle
    rectInd <- which(tm$x0 < coor[1] &
                       (tm$x0 + tm$w) > coor[1] &
                       tm$y0 < coor[2] &
                       (tm$y0 + tm$h) > coor[2])
    
    return(tm[rectInd[1], ])
    
  }



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  app.env <- new.env()
  output$condition <- renderText({
    condition()
  })
  
  output$tipoVisualizacion <- renderText({
    visualizacion()
  })
  
  output$tipoVisualizacion <- renderUI({
    input$filtro
    vector = c()
    v = visualizacion()
    if(v  == '1'){
      vector = c("Treemap" = "treemap",
                 "Tabla" = "tabla",
                 "Mapa departamental" = "Código.Departamento"
      )
    }else if(v == '0'){ 
      vector = c("Treemap" = "treemap",
                 "Tabla" = "tabla")
    }
    radioButtons("visualizacion","Escoja la forma de ver los datos",
                 choices = vector
    )
    #actionButton("", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
  })
  
  
  condition <- reactive({
    refresh = detalleGasto()
    retornoVisualizacion = input$visualizacion
    input$visualizacion
    result = 0
    if( !is.null(refresh)){
      if( is.null(retornoVisualizacion) || retornoVisualizacion != "Código.Departamento" ){
       if( refresh%%2 == 0 ){
          result = 0
         }else if( refresh%%2 == 1 && retornoVisualizacion == "treemap"  )
           result =1
        else if( refresh%%2 == 1 && retornoVisualizacion == "tabla")
          result = 3
          #gasto_tabla()
      }
        else if( retornoVisualizacion == "Código.Departamento")
          if( refresh%%2 == 0 ){
            result = 0
          }else if( retornoVisualizacion == "tabla" ){
           result = 3
          #gasto_tabla()
          }
          else
            result = 2
      }
    return(result)
  })
  
  
  visualizacion <- reactive({
    clasificacion = input$filtro
    result = 0
    if( !is.null(clasificacion) && clasificacion == "Código.Departamento"){
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
                             "Finalidad" = "Finalidad",
                             "Clasificación geográfica" = "Código.Departamento",
                             "Objeto del gasto" = "Objeto del gasto",
                             "Económico del gasto" = "Economico"
                 ), selected="Entidad"
                 )
    #actionButton("", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
  })
  
  

  
  detalleGasto=reactive({
    input$detalleGasto
  }) 
  
  
  gasto <- reactive({
    temp <- data.frame()
    if(!is.null(input$filtro)){
      temp <- entidad %>%
        select(Devengado, input$filtro)%>%
        group_by_(input$filtro) %>%
        summarise(devengado = sum(Devengado))
    }
    return(temp)
  })
  
  
  gastoTreeMap <- reactive({
    
    
    temp <- NULL
    nivel <- getRecordFromTreeMap()
    if( !is.null(input$filtro) ){
      switch (input$filtro,
              'Entidad' = {
                filtro = pop(jerarquia_institucional_ida)
                push(jerarquia_institucional_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_intitucional %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_intitucional %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_institucional_ida <<- jerarquia_institucional_ida
                jerarquia_institucional_regreso <<- jerarquia_institucional_regreso
                app.env$nivelActivo = filtro
              }, 
              'Finalidad' = {
                filtro = pop(jerarquia_finalidad_ida)
                push(jerarquia_finalidad_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_finalidad %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_finalidad %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_finalidad_ida <<- jerarquia_finalidad_ida
                jerarquia_finalidad_regreso <<- jerarquia_finalidad_regreso
                app.env$nivelActivo = filtro
              }, 
              'Código.Departamento' = {
                filtro = pop(jerarquia_geografico_ida)
                push(jerarquia_geografico_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_geografico %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_geografico %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_geografico_ida <<- jerarquia_geografico_ida
                jerarquia_geografico_regreso <<- jerarquia_geografico_regreso
                app.env$nivelActivo = filtro
              }, 
              'Objeto del gasto' = {
                filtro = pop(jerarquia_objeto_gasto_ida)
                push(jerarquia_objeto_gasto_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_objeto_gasto %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_objeto_gasto %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_objeto_gasto_ida <<- jerarquia_objeto_gasto_ida
                jerarquia_objeto_gasto_regreso <<- jerarquia_objeto_gasto_regreso
                app.env$nivelActivo = filtro
              }, 
              'Economico' = {
                filtro = pop(jerarquia_economico_ida)
                push(jerarquia_economico_regreso, paste0("'",filtro, "'") )
                if( is.null(nivel) || trimws(nivel) == "" ){
                  temp <- datos_economico %>%
                    select_(filtro, "Devengado") %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado)) 
                }else{
                  va <- app.env$nivelActivo
                  temp <- datos_economico %>%
                    select_(app.env$nivelActivo,  filtro, "Devengado") %>%
                    filter_( .dots = paste0(app.env$nivelActivo, "=='", nivel, "'")  ) %>%
                    group_by_(filtro) %>%
                    summarise(Devengado = sum(Devengado))
                }
                jerarquia_economico_ida <<- jerarquia_economico_ida
                jerarquia_economico_regreso <<- jerarquia_economico_regreso
                app.env$nivelActivo = filtro
              }
      )
    }
    return(temp)
  })
  
  output$treemap1 <- renderPlot({
    temp=gastoTreeMap()
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    variable <- names( temp )[1]
    if( !is.null(temp) ){
      .tm <<- treemap(temp,
                      index= variable,
                      vSize="Devengado",
                      vColor="Devengado",
                      type="value",
                      title = "",
                      palette="Purples",
                      border.col ="white",
                      position.legend="right",
                      fontsize.labels = 16,
                      title.legend="Escala de colores")
    }
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
    return(col)
    #filter(pop_data,Country==col)
  })
  
  output$mapa <- renderLeaflet({
  if(is.null(mapaJson)){
    withProgress(message = "Está cargando el mapa", value = 0, {
      mapaJson <<- rgdal::readOGR(dsn ="guatemala.geojson")
    })
  }
  datos <- gasto()
  if( nrow(datos) > 0  ){
    informacionMapa <- sp::merge(mapaJson, datos, by.x="Código.Departamento" )
    qpal <- colorQuantile("YlGn", informacionMapa$devengado, n = 5, na.color = "#bdbdbd")
    
    
    lat <- 16
    lng <- -89.5
    zoom <- 7
    
    mapa <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      #addTiles()%>%
      setView(lat = lat, lng = lng, zoom = zoom)%>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = informacionMapa, fillColor = ~qpal(devengado), fillOpacity = 0.7,
                  color = "white", weight = 2)
  }

    


  })
  
    output$tabla <- DT::renderDataTable({ 
      datos <- as.data.frame( gasto_tabla() )
      tabla_inutil <<- DT::datatable( tabla_temporal, options = list(orderClasses = TRUE) )
      })
    
    output$filtroTabla <- renderUI({
      radioButtons("opcionTabla","¿Qué filtro desea aplicar?",
                   choices = mascara_filtro_inicio, selected="Entidad", choiceValues = opciones_filtro_inicio , inline = T
      )
      #actionButton("", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
    })

    

    
    gasto_tabla <- function(filtro = '', variable = ''){
      if( is.null(datos_tabla)){
        withProgress(message ='Leyendo la información', value = 0, {
          datos_tabla <<- read.csv('datos_tabla.csv', sep = ';')  
        }) 
        
      }
      var <-  variable
      temporal <- NULL
      if( is.null(var) || var == '' ){
        temporal <- datos_tabla %>%
          summarise( Devengado = sum(Devengado) )%>%
          mutate(Concepto = 'Gasto Total') 
      }else{
        resultado <- tabla_temporal[var, "Concepto"] 
        if(resultado == "Gasto Total"){
          temporal <- datos_tabla %>%
            group_by_(filtro) %>%
            summarise(Devengado =  sum(Devengado))
          
          tabla_dinamica <<- datos_tabla
          temporal <- temporal %>%
            select_(filtro,"Devengado") %>%
            rename_(Concepto = filtro)
          
          dimension_ida <<- filtro
          #push(jerarquia_dimension_regreso,filtro)
          jerarquia_dimension_regreso <- c(jerarquia_dimension_regreso,filtro)
          #push(jerarquia_valor_dimension_regreso,"")
          jerarquia_valor_dimension_regreso <- "NADA"
          
          
          output$Atras <- renderUI({
            actionButton("retroceder_tabla", "Nivel Anterior")
          })
          
        }else{
          col <- as.character(tabla_temporal$Concepto[variable] )
          dimension_actual <- dimension_ida
          .dots <- list(interp(~y==x, .values = list( y = as.name(dimension_actual), x = col ) ))
          tabla_dinamica <<- tabla_dinamica %>%
            filter_( .dots = .dots )
          
          temporal <- tabla_dinamica %>%
            group_by_(filtro) %>%
            summarise(Devengado =  sum(Devengado) ) %>%
            rename_( Concepto = filtro )
          dimension_ida <<- filtro # quizás lo correcto es filtro actualmente es dimension_actual
          #push(jerarquia_dimension_regreso, dimension_actual)
          jerarquia_dimension_regreso <- c(jerarquia_dimension_regreso, dimension_actual)
          valor_dimension <<- col
          #push(jerarquia_valor_dimension_regreso, col)
          jerarquia_valor_dimension_regreso <- c(jerarquia_valor_dimension_regreso, col)
        }
        
        
      }
      


      
    tabla_temporal <<- temporal
    
    actualizarFiltro(filtro)

    jerarquia_valor_dimension_regreso <<- jerarquia_valor_dimension_regreso
    jerarquia_dimension_regreso <<- jerarquia_dimension_regreso
    
    return(temporal)
    }

    
    actualizarFiltro <-  function(filtro="") {
      if( !is.null( tabla_dinamica )  ){
        
        valores_filtros <- sapply(colnames(tabla_dinamica),function( x ){
          y <- tabla_dinamica[,x] 
          if( !is.numeric(y) ){
            y <- as.factor( as.character(y) )
            if( nlevels(y) >  1  ){
              return(x)
            }
          }
        })
        
        
        
        valores_filtros <- plyr::compact(valores_filtros)
        
        if (exists("filtro")) {
          valores_filtros <- valores_filtros[valores_filtros != filtro]
        }else{
          valores_filtros <- valores_filtros[valores_filtros != dimension_ida]
        }
        
        valores_filtros <- obtenerListaFiltros(valores_filtros)
        
        #View(valores_filtros)
        
        
        if(length(valores_filtros) > 0 )
          updateRadioButtons(session,"opcionTabla", choices = valores_filtros, choiceValues =  valores_filtros, inline = T) 
      }else{
        updateRadioButtons(session,"opcionTabla", choices = mascara_filtro_inicio, choiceValues =  opciones_filtro_inicio, inline = T) 
        }
      
    }
    
    construirTablaDinamica <- function(){
      tabla_temp <- NULL
      if( length(jerarquia_dimension_regreso  )  == 1 ){
        tabla_temp <- datos_tabla %>%
          summarise( Devengado = sum(Devengado) )%>%
          mutate(Concepto = 'Gasto Total') 
      }else{
        contador <- 2:length(jerarquia_dimension_regreso)
        for(z in  contador ){
          a <- jerarquia_dimension_regreso[[z]]
          b <- jerarquia_valor_dimension_regreso[[z]]
            .dots <- list(interp( ~y==x, .values = list( y = as.name(jerarquia_dimension_regreso[[z]] ), x = jerarquia_valor_dimension_regreso[[z]] ) ) )                      
            if( z == length(jerarquia_dimension_regreso) ) {
              if( z == 2 ){
                tabla_dinamica <<- datos_tabla 
                tabla_temp <- datos_tabla %>%
                  group_by_(jerarquia_dimension_regreso[[z]]) %>%
                  summarise( Devengado = sum(Devengado) ) %>%
                  rename_(Concepto = jerarquia_dimension_regreso[[z]])
                dimension_ida <<- jerarquia_dimension_regreso[[z]]
              }else{
              tabla_dinamica <<- tabla_temp
              tabla_temp <- tabla_temp %>%
               group_by_(jerarquia_dimension_regreso[[z]]) %>%
                summarise( Devengado = sum(Devengado) ) %>%
                rename_(Concepto = jerarquia_dimension_regreso[[z]])
                dimension_ida <<- jerarquia_dimension_regreso[[z]]
              }
            }else if( z == 2 ){
              tabla_temp <- datos_tabla %>%
                filter_(.dots = .dots)
            }else{
              tabla_temp <- tabla_temp %>%
                filter_(.dots = .dots)
            }
        }
      }
      #Quitando el ultimo elemento
      jerarquia_dimension_regreso <<- jerarquia_dimension_regreso[c(1:length(jerarquia_dimension_regreso) -1 )]
      jerarquia_valor_dimension_regreso <<- jerarquia_valor_dimension_regreso[c(1:length(jerarquia_valor_dimension_regreso) -1 )]
      #View(tabla_temp)
      tabla_temporal <<- tabla_temp
      #View(tabla_temporal)
      #View(tabla_dinamica)
    }
    
    
   obtenerListaFiltros <- function( valores ){
     listaExclusion <- NULL
     nombres <- names(valores)
     #View(nombres)
     if( "Sub.Grupo" %in%  nombres ){
       listaExclusion <- c(listaExclusion,"Entidad","Unidad.Ejecutora")
       nombres <- subset( nombres, !( nombres %in% c("Entidad","Unidad.Ejecutora") ) )
       
     }
     
     if( "Entidad" %in% nombres ){
       listaExclusion <- c(listaExclusion, "Unidad.Ejecutora" )
     }
     
     if( "Región" %in% nombres ){
       listaExclusion <- c(listaExclusion, c("Departamento", "Municipio") )
       nombres <- nombres[nombres != "Departamento"]
       nombres <- nombres[nombres != "Municipio"]
     }
     
     if( "Departamento" %in% nombres ){
       listaExclusion <- c(listaExclusion,  "Municipio" )
     }
     
     if( "Finalidad" %in% nombres ){
       listaExclusion <- c(listaExclusion, c("Función", "División") )
       nombres <- nombres[nombres != "Función"]
       nombres <- nombres[nombres != "División"]
     }
     
     
     if( "Función" %in% nombres ){
       listaExclusion <- c(listaExclusion,  "División" )
     }
     
     if( "Grupo.Gasto" %in% nombres ){
       listaExclusion <- c(listaExclusion, c("Sub.Grupo.Gasto", "Renglón") )
       nombres <- nombres[nombres != "Sub.Grupo.Gasto"]
       nombres <- nombres[nombres != "Renglón"]
     }
     
     if( "Sub.Grupo.Gasto" %in% nombres ){
       listaExclusion <- c(listaExclusion,  "Renglón" )
     }
     
     
     if( "Económico.Nivel.1" %in% nombres ){
       listaExclusion <- c(listaExclusion, c("Económico.Nivel.2", "Económico.Nivel.3","Económico.Nivel.4","Económico.Nivel.Operativo") )
       nombres <- subset( nombres, !( nombres %in% c("Económico.Nivel.2","Económico.Nivel.3","Económico.Nivel.4","Económico.Nivel.Operativo") ) )
    }
     
     if( "Económico.Nivel.2" %in% nombres ){
       listaExclusion <- c(listaExclusion, c( "Económico.Nivel.3","Económico.Nivel.4","Económico.Nivel.Operativo") )
       nombres <- subset(nombres,  !(nombres %in% c("Económico.Nivel.3","Económico.Nivel.4","Económico.Nivel.Operativo") ) ) 
     }
     
     if( "Económico.Nivel.3" %in% nombres ){
       listaExclusion <- c(listaExclusion, c("Económico.Nivel.4","Económico.Nivel.Operativo") )
       nombres <- subset(nombres,  !( nombres %in% c("Económico.Nivel.4","Económico.Nivel.Operativo") ) )
     }
     
     
     if( "Económico.Nivel.4" %in% nombres ){
       listaExclusion <- c(listaExclusion, "Económico.Nivel.Operativo" )
     }
     
     if( "Programa" %in% nombres ){
       listaExclusion <- c(listaExclusion, c("Sub.Programa", "Proyecto", "Actividad", "Obra") )
       nombres <- subset(nombres,  !( nombres %in% c("Sub.Programa","Proyecto", "Actividad", "Obra") ) )
     }
     
     if( "Sub.Programa" %in% nombres ){
       listaExclusion <- c(listaExclusion, c("Proyecto", "Actividad", "Obra") )
       nombres <- subset(nombres,  !( nombres %in% c("Proyecto", "Actividad", "Obra") ) )
     }
     
     if( "Proyecto" %in% nombres ){
       listaExclusion <- c(listaExclusion, c( "Actividad", "Obra") )
       nombres <- subset(nombres,  !( nombres %in% c("Actividad", "Obra") ) )
     }
     
     if( "Actividad" %in% nombres ){
       listaExclusion <- c(listaExclusion, "Obra" )
     }
     
     
     nombres <- names(valores)[-which( names(valores) %in% listaExclusion )]
     return(nombres)
     
   }
    
    
    output$Avanzar <- renderUI({
      actionButton("avanzar_tabla", "Siguiente nivel")
    })

    observeEvent(input$avanzar_tabla, {
      filtro <- input$opcionTabla
      variable <- input$tabla_rows_selected
      tablita <- gasto_tabla(filtro, variable)
      output$tabla <- DT::renderDataTable({ 
        datos <- as.data.frame( tablita )
        tabla_inutil <<- DT::datatable( tabla_temporal, options = list(orderClasses = TRUE) )
        })
      })
    
    
    observeEvent(input$retroceder_tabla, {
      tablita <- construirTablaDinamica()
      output$tabla <- DT::renderDataTable({ 
        datos <- as.data.frame( tablita )
        tabla_inutil <<- DT::datatable( tabla_temporal, options = list(orderClasses = TRUE) )
      })
      
      actualizarFiltro()
      
      
      
      
      
    })

      
      
      

  
})
