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
datos_tabla <- read.csv('datos_tabla.csv', sep = ';')
mapaJson <-  rgdal::readOGR(dsn ="guatemala.geojson")
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


dimension_ida = NULL
valor_dimension = NULL
jerarquia_dimension_regreso = lifo()
jerarquia_valor_dimension_regreso = lifo()


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
shinyServer(function(input, output) {
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
    print(retornoVisualizacion)
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
          gasto_tabla()
      }
        else if( retornoVisualizacion == "Código.Departamento")
          if( refresh%%2 == 0 ){
            result = 0
          }else if( retornoVisualizacion == "tabla" ){
           result = 3
          gasto_tabla()
          }
          else
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
    print( paste("La opción dada por el filtro es ",input$filtro )  )
    temp <- entidad %>%
      select(Devengado, input$filtro)%>%
      group_by_(input$filtro) %>%
      summarise(devengado = sum(Devengado))
    return(temp)
  })
  
  
  gastoTreeMap <- reactive({
    temp <- NULL
    nivel <- getRecordFromTreeMap()
    print( paste("El dato obtenido es:",  nivel) )
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
          print( paste("Los datos relevantes son: ", app.env$nivelActivo, filtro, nivel) )
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
          print( paste("Los datos relevantes son: ", app.env$nivelActivo, filtro, nivel) )
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
          print( paste("Los datos relevantes son: ", app.env$nivelActivo, filtro, nivel) )
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
          print( paste("Los datos relevantes son: ", app.env$nivelActivo, filtro, nivel) )
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
          print( paste("Los datos relevantes son: ", app.env$nivelActivo, filtro, nivel) )
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
    print(temp)
    return(temp)
  })
  
  output$treemap1 <- renderPlot({
    temp=gastoTreeMap()
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    variable <- names( temp )[1]
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
    print(paste("El centro es: ", treemap_clicked$center) )
    
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
  informacionMapa <- sp::merge(mapaJson, gasto(), by.x="Código.Departamento" )
  print(gasto())
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
    


  })
  
    output$tabla <- DT::renderDataTable({ 
      datos <- as.data.frame( gasto_tabla() )
      #gasto_tabla()
      print(paste("La tabla temporal es: ", tabla_temporal))
      tabla_inutil <<- DT::datatable( tabla_temporal, options = list(orderClasses = TRUE) )
      })
    
    output$filtroTabla <- renderUI({
      radioButtons("opcionTabla","¿Qué filtro desea aplicar?",
                   choices = c("Institución" = "Entidad",
                               "Finalidad" = "Finalidad",
                               "Clasificación geográfica" = "Departamento",
                               "Objeto del gasto" = "Objeto del gasto",
                               "Económico del gasto" = "Economico"
                   ), selected="Entidad", inline = T
      )
      #actionButton("", em("Ver detalle del gasto",style="text-align:center;color:blue;font-size:200%"))
    })

    

    
    gasto_tabla <- function(filtro = '', variable = ''){
      print( paste("La opción dada por el filtro es ",filtro )  )
      print( paste("La opción dada por la variable es ",variable )  )
      var <-  variable
      print( paste("La fila seleccionada es: ",  var) )
      temporal <- NULL
      if( is.null(var) || var == '' ){
        temporal <- datos_tabla %>%
          summarise( Devengado = sum(Devengado) )%>%
          mutate(Concepto = 'Gasto Total') 
      }else{
        resultado <- tabla_temporal[var, "Concepto"] 
        print(resultado)
        if(resultado == "Gasto Total"){
          temporal <- datos_tabla %>%
            group_by_(filtro) %>%
            summarise(Devengado =  sum(Devengado))
          
          tabla_dinamica <<- datos_tabla
          temporal <- temporal %>%
            select_(filtro,"Devengado") %>%
            rename_(Concepto = filtro)
          
          dimension_ida <<- filtro
          push(jerarquia_dimension_regreso,filtro)
          push(jerarquia_valor_dimension_regreso,"")
          
          
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
          push(jerarquia_dimension_regreso, dimension_actual)
          dimension_ida <<- dimension_actual
          valor_dimension <<- col
          push(jerarquia_valor_dimension_regreso, col)  
        }
      }
      
    tabla_temporal <<- temporal

    print( paste("La tabla dinamica es : ", dim(tabla_dinamica) ))
    if( !is.null( tabla_dinamica )  ){
      
      valores_filtros <- sapply(colnames(tabla_dinamica),function( x ){
        y <- tabla_dinamica[,x] 
        if( !is.numeric(y) ){
          y <- as.factor( as.character(y) )
          if( nlevels(y) >  1  ){
            print(x)
            return(x)
          }
        }
      })
      valores_filtros <- plyr::compact(valores_filtros)
      
      valores_filtros <- obtenerListaFiltros(valores_filtros)

      
      
      if (exists("filtro")) {
        print(paste(" Si existe el filtro y es ", filtro))
        valores_filtros <- valores_filtros[valores_filtros != filtro]
      }else{
        valores_filtros <- valores_filtros[valores_filtros != dimension_ida]
      }
      print( paste("Los posibles filtros son:", valores_filtros   ) )
    }
    jerarquia_valor_dimension_regreso <<- jerarquia_valor_dimension_regreso
    jerarquia_dimension_regreso <<- jerarquia_dimension_regreso
    
    View(tabla_dinamica)
    return(temporal)
    }

   obtenerListaFiltros <- function( valores ){
     listaExclusion <- NULL
     nombres <- names(valores)
     if( "Grupo" %in%  nombres ){
       listaExclusion <- c(listaExclusion,"Sub.Grupo")
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
     
     
     nombres <- nombres[-which( nombres %in% listaExclusion )]
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
        #gasto_tabla()
        print(paste("La tablita es : ", names(tablita)[1] ) )
        print(paste("La tabla temporal es: ", tabla_temporal))
        tabla_inutil <<- DT::datatable( tabla_temporal, options = list(orderClasses = TRUE) )
      })
      



    })
  
  
})
