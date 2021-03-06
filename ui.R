#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(leaflet)
#require(DT)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    selectInput("year","Ejercicio fiscal" ,choices = c(1998:format(Sys.Date(), "%Y")), selected = format(Sys.Date(), "%Y")  ),
    uiOutput("opcionesFiltro"),
    uiOutput("tipoVisualizacion")
  ),
  dashboardBody(
    conditionalPanel(
      condition = "output.condition == 0",
      fluidRow(
      infoBoxOutput("progressBox"),
      infoBoxOutput("devengadoBox"),
      uiOutput("detalle")
      )
      ),
      
    conditionalPanel(
      condition = "output.condition == 1",
      fluidRow(
        br(),
        br(),
        tags$h1("Detalle del Gasto ",style="text-align:center;color:blue;font-size:200%"),
        tags$p("Click On Any Region To Get The Treemap Of That Region",style="text-align:center;color:purple"),
        plotOutput("treemap1",height="600px", click = "click_treemap")
      )
    ),
    # ,
    # 
    conditionalPanel(
      condition = "output.condition == 2",
      fluidRow(
        br(),
        br(),
        tags$h1("Mapa de Guatemala"),
        leafletOutput("mapa", height = "600")
      )
    ),
    conditionalPanel(
      condition = "output.condition == 3",
      fluidRow(
        br(),
        br(),
        uiOutput("filtroTabla"),
        tags$h1("Visualización con tabla"),
        DT::dataTableOutput("tabla"),
        uiOutput("Avanzar"), 
        uiOutput("Atras"),
        uiOutput("Hierarchy"),
        d3treeOutput(outputId="d3",width = '1200px',height = '800px')
      )
    )
    
    
    


    )
    
  
)