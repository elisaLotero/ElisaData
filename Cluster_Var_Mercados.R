library(maditr)
library(tidyverse)
library(dplyr)

library("FactoMineR")
library("factoextra")

###SELECCIONAMOS EL DIRECTORIO Y LA BASE DE DATOS 
data <- read.csv("https://raw.githubusercontent.com/elisaLotero/ElisaData/main/bd_mercados_csv.csv", header = TRUE)

##DEPURAMOS LOS DATOS 
###sacamos la venta comuniatria  
data.mercados <- data %>%
  filter(!(tipo.mercado.venta == "comunitario"))

###seleccionamos las variables para hacer el MCA
data.mercados <- data.mercados %>% select(ï..comunidad, num.entrevista, tipo.mercado.venta, no.productos, distancia, tiempo, tipo.de.carretera)%>% 
  distinct()

##vamos a darle valores a la cantidad de productos vendidos 
#summary(data.mercados$no.productos)

data.mercados$prop.venta <- 0

for (i in 1:dim(data.mercados)[1]){
  if (data.mercados$no.productos[i] <= 7){
    data.mercados$prop.venta[i] <- "bajo"
  } else {
    if (data.mercados$no.productos[i] <= 12){
      data.mercados$prop.venta[i] <- "medio"
    } else {
      data.mercados$prop.venta[i] <- "alto"
    }
  }
}

#quitamos la counidad y el no. de entrevista 
#data.mercados <- data.mercados[ , 3:8]

data.mercados$conteo<-1


#sumamos los productos de los mercados que son iguales
data.mercados <- data.mercados %>% group_by(tipo.mercado.venta, tipo.de.carretera, distancia, tiempo, prop.venta)%>%
  summarise(no.entrevista = sum(no.productos))


##arreglamos los datos para que no se onfundan al dcastear
data.mercados$tipo.mercado.venta<-paste("mercado", data.mercados$tipo.mercado.venta, sep="_")
data.mercados$distancia<-paste("distancia", data.mercados$distancia, sep="_")
data.mercados$tiempo<-paste("tiempo", data.mercados$tiempo, sep="_")
data.mercados$prop.venta<-paste("venta", data.mercados$prop.venta, sep="_")

data.mercados <- data.mercados %>% distinct()


#anyNA(data.mercados)

###agregamos una columna de unos para hacer un conteo y otra con una secuancia para dar identidad a cada linea
data.mercados$conteo<-1
data.mercados$id  <- seq(1:dim(data.mercados)[1])

#dcasteamos
newdata.mercados<- cbind(dcast(data.mercados, no.entrevista + id ~ tipo.mercado.venta, value.var = "conteo"),
                         dcast(data.mercados, no.entrevista + id ~ tipo.de.carretera, value.var = "conteo")[ ,-c(1:2)],
                         dcast(data.mercados, no.entrevista + id ~ distancia, value.var = "conteo")[ ,-c(1:2)],
                         dcast(data.mercados, no.entrevista + id ~ tiempo, value.var = "conteo")[ ,-c(1:2)],
                         dcast(data.mercados, no.entrevista + id ~ prop.venta, value.var = "conteo")[ ,-c(1:2)])

newdata.mercados[is.na(newdata.mercados)] <- 0

###remplazamos los 1 con el valor del numero de productos correspondiente a cada linea
for (i in 1:dim(newdata.mercados)[1]){
  for (j in 3:dim(newdata.mercados)[2]){
    if (newdata.mercados[[i,j]] == 1){
      newdata.mercados[[i,j]] <- newdata.mercados$no.entrevista[i]}#el doble corchete es para que entienda 
    #ques es solo ese valor
  }
}

#la dejamos como una matriz de valores
#newdata.mercados <- newdata.mercados %>%
  #select(!c(mixta, "mercado_micro-regional"))

newdata.mercados.dis <- newdata.mercados [ ,3:18] #%>% ungroup () %>% dplyr::select(!c(ï..comunidad, num.entrevista, prop.factor))

library(ClustOfVar)

##PCA
PCA(newdata.mercados.dis)

##Validamos el PCA
library(psych)
cortest.bartlett(newdata.mercados.dis)

# Hacemos el dendograma 
variable_tree <- hclustvar(X.quanti = newdata.mercados.dis) # puede er quanti o quali (em este caso son valores cuantitativos)

#Graficamos
#par(mfrow=c(1,2))
plot(variable_tree, type = "index")
plot(variable_tree)
#dev.off()

#stability(variable_tree, B=24)

plot(variable_tree, type = "index")

clus <- cutreevar(variable_tree,3)
## print the list of variables in each cluster groups
print(clus$var)

#save.image(file="variable_tree")????

#ggsave("variable_tree_manejo.png", plot = variable_tree)
