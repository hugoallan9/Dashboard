
require(treemap)
require(dplyr)
require(shiny)
require(gridBase)
require(RColorBrewer)
require(plotly)
require(leaflet)
require(flifo)
require(lazyeval)
library(d3Tree)


ejecucionMes <- read.csv('EjecucionMensual.csv')
entidad <- read.csv('Sabana_de_prueba_TF.csv', sep =  ';')
datos_intitucional <- read.csv('Jerarquia_Entidad.csv', sep=";")
datos_finalidad <- read.csv('Jerarquia_Finalidad.csv', sep= ';')
datos_geografico <- read.csv('Jerarquia_Geografico.csv', sep=';')
datos_objeto_gasto <- read.csv('Jerarquia_ObjetoGasto.csv', sep=';')
datos_economico <- read.csv('Jerarquia_Economico.csv', sep=';')



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