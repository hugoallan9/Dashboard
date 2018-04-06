library(data.tree)


l <- jerarquia_entidad$ToList()

jerarquia_entidad <- read.csv('/mnt/Datos/GitHub/GastoAdmoCentral/Jerarquia_Entidad.csv', sep = ';')
# jerarquia_entidad$pathString <- paste("Dato", jerarquia_entidad$Sector, jerarquia_entidad$Sub.Sector, jerarquia_entidad$Grupo, jerarquia_entidad$Sub.Grupo, jerarquia_entidad$Entidad, jerarquia_entidad$Unidad.Ejecutora, sep = "/ name:")
# tree <- as.Node(jerarquia_entidad[,])
# 
# print(tree, pruneMethod = "dist", limit = 20)
# 
# 
# l <- data.tree::ToListExplicit(tree)
# 

library(rjson)
jerarquia_entidad <- toJSON(l)

write(jerarquia_entidad, file = '/mnt/Datos/GitHub/GastoAdmoCentral/jerarquia_institucional.json')



require(RJSONIO)



makeList<-function(x){
  if(ncol(x)>2){
    x2 <- dplyr::filter(x, x[1] != "")
    listSplit<-split(x[-1],x[1],drop=T)
    lapply(names(listSplit),function(y){list(name=y,children=makeList(listSplit[[y]]))})
  }else{
    lapply(seq(nrow(x[1])),function(y){list(name=x[,1][y],size=x[,2][y])})
  }
}


jsonOut<-toJSON(list(name="MyData",children=makeList(jerarquia_entidad[-1])))
cat(jsonOut)
write(jsonOut, file = '/mnt/Datos/GitHub/GastoAdmoCentral/jerarquia_institucional.json')
