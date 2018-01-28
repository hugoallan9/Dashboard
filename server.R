#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
ejecucionMes <- read.csv('EjecucionMensual.csv')



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$condition <- renderText({
    condition()
  })
  
  condition <- reactive({
    refresh = detalleGasto()
    print(refresh)
    if( !is.null(refresh) ){
      if( refresh%%2 == 0){
        result = 0
      }else if(refresh%%2 == 1){
        result =1
      }else{
        result = 0
      }
    }
    #return(result)
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
  
  
  
})
