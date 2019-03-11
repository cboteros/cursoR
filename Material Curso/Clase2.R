#Llamamos a la libreria dplyr
library(dplyr)

#llamamos la libreria "tidyr" la cual sirve para convertir fechas
library(tidyr)

datos<-read.csv("C:/Users/david.espinosa/Desktop/Diplomado/Ejercicio1.csv",sep=",",header = TRUE,encoding = "UTF-8")


#Cuantos regalos debemos de entregar
#el total de usuarios que poseemos en BD
nrow(datos)

#cuales son sus productoctos
ftable(datos[c("Referencia_producto","Ciudad")])

ftable(datos[c("Ciudad","Referencia_producto")])

#la funsión summarise crea un nuevo Data Frame
#producto en cada ciudad que más ventas obtuvo
grupos <- 
  group_by(datos, Ciudad, Referencia_producto)
  summarise(grupos, valor_ciuda_referencia = sum(Valor_Compra))
  
#Este me crea un nuevo data frame
grupos <- 
    group_by(datos, Ciudad, Referencia_producto)%>%
  summarise( valor_ciuda_referencia = sum(Valor_Compra))

#Este me srirvecon el fin de filtrar

plot(datos$Referencia_producto, main = "Cantidad de productos comprados", 
     col = "dark red", xlab = "Referencia producto", ylab = "Número de productos")

#Analisi



