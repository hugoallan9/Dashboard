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

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    conditionalPanel(
      condition = "output.condition == 0",
      fluidRow(
      selectInput("year","Ejercicio fiscal" ,choices = c(1998:format(Sys.Date(), "%Y")), selected = format(Sys.Date(), "%Y")  ),
      infoBoxOutput("progressBox"),
      infoBoxOutput("devengadoBox"),
      uiOutput("detalle")
      ) 


    )
    
  )
)