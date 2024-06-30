##AGRUPACIÓN POR ENTREVISTA

#install.packages(c("FactoMineR", "factoextra"))

library(maditr)
library(tidyverse)
library(dplyr)

library("FactoMineR")
library("factoextra")

###SELECCIONAMOS EL DIRECTORIO Y LA BASE DE DATOS 
#setwd("~/DOCTORADO/TESIS/datos/R/bd/")
data<-read.csv("https://github.com/elisaLotero/ElisaData.git/bd_completa_csv.csv", header = TRUE)

data.manejo <- data %>% select(c(ï..comunidad, num.entrevista, 
                                  nombre.recurso,manejo, 
                                  tipo.terreno,
                                  riego, manejo.producto, 
                                  manejo.herramienta, manejo.estructura)) %>%
  distinct()

#Eliminamos las filas con NA de la columna tipo.terreno 
#porque esas son las que se compran
data.manejo<- data.manejo %>% drop_na(manejo)

###creamos una base de datos con los datos unicos 
data.manejo <- data.manejo %>% distinct()

data.manejo <- data.manejo[ , -c(3)]
data.manejo <- data.manejo %>% distinct()

#cambiamos los NA
data.manejo$tipo.terreno[is.na(data.manejo$tipo.terreno)] <- "NA_terreno"
data.manejo$riego[is.na(data.manejo$riego)] <- "NA_riego"
data.manejo$manejo.producto[is.na(data.manejo$manejo.producto)] <- "no_usa_productos"
data.manejo$manejo.estructura[is.na(data.manejo$manejo.estructura)] <- "NA_estructura"


####homologamos términos
#unique(data.manejo$manejo.producto)
data.manejo$manejo.producto[data.manejo$manejo.producto == "abono_animal"]<- "insumo"
data.manejo$manejo.producto[data.manejo$manejo.producto == "abono_plantas"]<- "insumo"
data.manejo$manejo.producto[data.manejo$manejo.producto == "abono_quimico+abono_animal"]<- "insumo"
data.manejo$manejo.producto[data.manejo$manejo.producto == "abono_quimico+abono_plantas"]<- "insumo"
data.manejo$manejo.producto[data.manejo$manejo.producto == "abono_quimico"]<- "insumo"
data.manejo$manejo.producto[data.manejo$manejo.producto == "cal"]<- "insumo"
data.manejo$manejo.producto[data.manejo$manejo.producto == "plaguicida"]<- "insumo"

#unique(data.manejo$riego)
data.manejo$riego[data.manejo$riego == "manguera"]<- "riego"
data.manejo$riego[data.manejo$riego == "temporal+manguera"]<- "riego"
data.manejo$riego[data.manejo$riego == "NA_riego"]<- "temporal"

#unique(data.manejo$manejo.herramienta)
data.manejo$manejo.herramienta[data.manejo$manejo.herramienta == "manual+animal"]<- "herramienta_animal"

#unique(data.manejo$manejo.estructura)
data.manejo$manejo.estructura[data.manejo$manejo.estructura == "rotacion_cultivo"]<- "manejo_espacio"
data.manejo$manejo.estructura[data.manejo$manejo.estructura == "descanso_tierra"]<- "manejo_espacio"
data.manejo$manejo.estructura[data.manejo$manejo.estructura == "division_cultivo"]<- "manejo_espacio"



data.manejo <- data.manejo %>% distinct()

data.manejo$conteo<-1

###hacemos un summerise del conteo para que me cuente cuantos productos son manejados 
###dentro de cada combinación de variables 
data.manejo <- data.manejo %>% group_by(manejo, 
                                        tipo.terreno, 
                                        riego, manejo.producto, 
                                        manejo.herramienta, manejo.estructura)%>%
  summarise(no.entrevistas = sum(conteo))

data.manejo$conteo<-1
data.manejo$id  <- seq(1:dim(data.manejo)[1])

#dcasteamos
newdata.manejo<- cbind(dcast(data.manejo, no.entrevistas + id  ~ tipo.terreno, value.var = "conteo"),
                       dcast(data.manejo, no.entrevistas +id  ~ manejo, value.var = "conteo")[ ,-c(1:2)],
                       dcast(data.manejo, no.entrevistas +id  ~ riego, value.var = "conteo")[ ,-c(1:2)],
                       dcast(data.manejo, no.entrevistas + id  ~ manejo.producto, value.var = "conteo")[ ,-c(1:2)],
                       dcast(data.manejo, no.entrevistas + id  ~ manejo.herramienta, value.var = "conteo")[ ,-c(1:2)],
                       dcast(data.manejo, no.entrevistas + id  ~ manejo.estructura, value.var = "conteo")[ ,-c(1:2)])
#dcast(data.manejo, no.productos + id  ~ tamano.cultivo, value.var = "conteo")[ ,-c(1:2)],
#dcast(data.manejo, no.productos + id  ~ terreno.propiedad, value.var = "conteo")[ ,-c(1:2)])

newdata.manejo[is.na(newdata.manejo)] <- 0

#Ponemos invernadero como manejo de espacio
newdata.manejo$manejo_del_espacio = newdata.manejo$manejo_espacio + 
  newdata.manejo$invernadero

newdata.manejo <- newdata.manejo %>% select(!c(manejo_espacio, invernadero))
#unique(newdata.manejo$manejo_del_espacio)

#Ponemos herramienta animal como insumos
newdata.manejo$insumos = newdata.manejo$insumo + 
  newdata.manejo$herramienta_animal

newdata.manejo <- newdata.manejo %>% select(!c(herramienta_animal, insumo))
#unique(newdata.manejo$insumos)
newdata.manejo$insumos[newdata.manejo$insumos == 2]<- 1

#reemplazamos 1 y 0 por el # de entrevistas
for (i in 1:dim(newdata.manejo)[1]){
  for (j in 3:dim(newdata.manejo)[2]){
    if (newdata.manejo[[i,j]] == 1){
      newdata.manejo[[i,j]] <- newdata.manejo$no.entrevistas[i]}#el doble corchete es para que entienda 
    #ques es solo ese valor
  }
}

newdata.manejo <- newdata.manejo %>% select(!c(temporal, no_usa_productos, 
                                               manual, NA_estructura))
newdata.manejo.dis <- newdata.manejo [ , c(3:13)] #%>% ungroup () %>% dplyr::select(!c(ï..comunidad, num.entrevista, prop.factor))

#eliminamos las columanas de NA y las que no son de utilidad

library(ClustOfVar)

##PCA
PCA(newdata.manejo.dis)

# Hacemos el dendograma 
variable_tree <- hclustvar(X.quanti = newdata.manejo.dis) # puede er quanti o quali (em este caso son valores cuantitativos)

##graficamos
#par(mfrow=c(1,2))
PCA(newdata.manejo.dis)
#stability(variable_tree, B=24)
#plot(variable_tree, type = "index")
plot(variable_tree)
#dev.off()

clus <- cutreevar(variable_tree,3)
## print the list of variables in each cluster groups
print(clus$var)

#save.image(file="variable_tree")????

#ggsave("variable_tree_manejo.png", plot = variable_tree)

