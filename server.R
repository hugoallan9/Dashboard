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

ejecucionMes <- read.csv('EjecucionMensual.csv')
entidad <- read.csv('Entidad.csv')


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$condition <- renderText({
    condition()
  })
  
  condition <- reactive({
    refresh = detalleGasto()
    print(refresh)
    result = 0
    if( !is.null(refresh) ){
      if( refresh%%2 == 0){
        result = 0
      }else if(refresh%%2 == 1){
        result =1
      }else{
        result = 0
      }
    }
    print(paste("El resultado es: ", result))
    return(result)
  })
  
  output$condition <- renderText({
    condition()
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
  
  detalleGasto=reactive({
    input$detalleGasto
  })
  
  
  gasto <- reactive({
    entidad %>%
      select(Ejercicio, Devengado, Entidad) %>%
      filter(Ejercicio == input$year) %>%
      group_by(Entidad) %>%
      summarise(devengado = sum(Devengado))
  })
  
  output$treemap1 <- renderPlot({ 
    
    par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
    plot(c(0,1), c(0,1),axes=F, col="white")
    vps <- baseViewports()
    
    temp=gasto()
    .tm <-   treemap(temp, 
                    index="Entidad", 
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
  
  
  
})
