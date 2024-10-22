library(maditr)
library(tidyverse)
library(dplyr)

library("FactoMineR")
library("factoextra")


# Seleccionamos la base de datos 
data <- read.csv("https://raw.githubusercontent.com/elisaLotero/ElisaData/main/bd_completa_csv.csv", header = TRUE)

#Seleccionamos las variables que necesitamos (socieconomicas)
dataSE <- data %>%
  select(c(ï..comunidad, num.entrevista, terreno.propiedad, 
            escolaridad, lengua, origen, migracion, #campesino
            pastor, comercio,
            tendero, productor, jornalero, otro
            ))


dataSE <- dataSE %>% filter(!(dataSE$terreno.propiedad == "comunal"))
#dejamos solo las filas unicas
dataSE <- dataSE %>% distinct()

#cambiamos los NA


dataSE$escolaridad[is.na(dataSE$escolaridad)] <- "No_estudio"
dataSE$escolaridad[dataSE$escolaridad== "no"] <- "No_estudio"
dataSE$terreno.propiedad[is.na(dataSE$terreno.propiedad)] <- "No_tiene_propiedad"

#unique(dataSE$lengua)
dataSE$lengua[dataSE$lengua == "espanol" | dataSE$lengua == "bilingue"] <- "habla_espanol"

#unique(dataSE$terreno.propiedad)
dataSE$terreno.propiedad[dataSE$terreno.propiedad == "propio"] <- "Propietario" 
dataSE$terreno.propiedad[dataSE$terreno.propiedad == "rentado"] <- "No_tiene_propiedad"
dataSE$terreno.propiedad[dataSE$terreno.propiedad == "ejidal"] <- "Propietario"

#unique(dataSE$escolaridad)
dataSE$escolaridad[dataSE$escolaridad == "primaria" | dataSE$escolaridad == "secundaria" |
                   dataSE$escolaridad == "bachiller"] <- "Escuela" 

#unique(dataSE$migracion)
dataSE$migracion[dataSE$migracion == "nacional" |
                           dataSE$migracion == "internacional"] <- "Migracion"
dataSE$migracion[dataSE$migracion == 0] <- "No_migracion"

#unificamos variables sumando columnas
dataSE$productor_1rio = dataSE$pastor + dataSE$jornalero#+ dataSE$campesino
dataSE <- dataSE %>% select(!c(pastor#, #comercio, #campesino
                               ))
#unique(dataSE$productor_1rio)
dataSE$productor_1rio[dataSE$productor_1rio == 2 | dataSE$productor_1rio == 3]<-1  

dataSE$productor_2rio =  dataSE$tendero + dataSE$comercio
dataSE <- dataSE %>% select(!c(tendero, jornalero))
#unique(dataSE$productor_2rio)
dataSE$productor_2rio[dataSE$productor_2rio == 2] <- 1 

dataSE$productor_3rio = dataSE$otro + dataSE$productor 
dataSE <- dataSE %>% select(!c(otro, productor))
#unique(dataSE$productor_3rio)
dataSE$productor_3rio[dataSE$productor_3rio == 2] <- 1

dataSE <- dataSE %>% distinct()

dataSE$conteo<-1

###hacemos un summerise del conteo para que me cuente cuantos productos son manejados 
###dentro de cada combinación de variables 
dataSE <- dataSE %>% group_by(ï..comunidad, num.entrevista, escolaridad, lengua, origen, 
                              migracion, productor_1rio, productor_2rio, productor_3rio)%>%
  summarise(propiedad = first(terreno.propiedad))




#anyNA(dataSE)

###agregamos una columna de unos para hacer un conteo y otra 
#con una secuancia para dar identidad a cada linea
dataSE$conteo<-1

#dcasteamos

dataSE<-unite(dataSE, id, ï..comunidad:num.entrevista,  sep="_", remove = FALSE)

dataSE$num.entrevista <- NULL


newdata.SE <- cbind(dcast(dataSE, id +  productor_1rio + productor_2rio + productor_3rio ~ propiedad , value.var = "conteo"), 
                    dcast(dataSE, id +  productor_1rio + productor_2rio + productor_3rio ~ escolaridad, value.var = "conteo")[ ,-c( 1:4)],
                    dcast(dataSE, id +  productor_1rio + productor_2rio + productor_3rio ~ lengua, value.var = "conteo")[ ,-c( 1:4)],
                    dcast(dataSE, id +  productor_1rio + productor_2rio + productor_3rio ~ origen, value.var = "conteo")[ ,-c( 1:4)],
                    dcast(dataSE, id +  productor_1rio + productor_2rio + productor_3rio ~ migracion, value.var = "conteo")[ ,-c( 1:4)],
                    dcast(dataSE, id +  productor_1rio + productor_2rio + productor_3rio ~ ï..comunidad, value.var = "conteo")[ ,-c( 1:4)])

#dejamos todo en matríz de 1 y 0
#newdata.SE[ ,3:27] <- (newdata.SE[ ,3:27]/newdata.SE[ ,3:27])
newdata.SE[is.na(newdata.SE)] <- 0

newdata.SE <- newdata.SE %>%
  select(!c(No_migracion, Propietario, municipal, nacional, habla_espanol,
            Escuela
            ))

#seleccionamos solo las columnas que necesitamos 
newdata.SE.dis <- newdata.SE [ ,c(2:13)]

library(ClustOfVar)

##PCA
PCA(newdata.SE.dis)

##Validamos el PCA
library(psych)
cortest.bartlett(newdata.SE.dis[ ,1:8])

# Hacemos el dendograma 
variable_tree <- hclustvar(X.quanti = newdata.SE.dis) # puede er quanti o quali (em este caso son valores cuantitativos)

#Graficamos
#par(mfrow=c(1,2))
#plot(variable_tree, type = "index")
plot(variable_tree)
#dev.off()

clus <- cutreevar(variable_tree,3)
## print the list of variables in each cluster groups
print(clus$var)

