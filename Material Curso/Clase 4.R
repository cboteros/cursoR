######OBJETIVO######

#Entender el comportamiento de los usuarios de esta marca


############## SELECCIONAR EL CONJUNTO DE DATOS A ANALIZAR ################

clientes<-
  read.csv("C:/Users/USUARIO/Desktop/Diplomado/Clase 4/clientes.csv",sep= ";", header = TRUE)

transacciones<-
  read.csv("C:/Users/USUARIO/Desktop/Diplomado/Clase 4/Transacciones.csv", sep= ";", header= TRUE)

library(dplyr)

#library(stats)
#library(reshape)
# 1.Reconocimiento de las variables existentes
# 2.Identificaci?n de nuevas variables segun las que ya poseemos

# ?Como creamos una nueva variable?


clientes$vector_unos<-
  as.numeric(1)

#clientes<-
  #clientes[,-10]

# ?Como creemos la variable edad? 

install.packages(tidyr) # Convierte el formato de la variable en fecha
library(tidyr)

clientes$Fecha_Nacimiento<-
  as.Date(clientes$Fecha_Nacimiento, "%d/%m/%Y")# Convierte el formato de la variable en fecha
  class(clientes$Fecha_Nacimiento)# Identificar el formato de la variable

clientes$fecha_hoy<-
  as.Date(Sys.Date(),"%d/%m/%Y") #Creamos una nueva variable con la fecha de hoy

clientes$edad<-
  as.numeric((clientes$fecha_hoy-clientes$Fecha_Nacimiento)/368.25)

clientes$edad<-
  as.numeric(round((clientes$fecha_hoy-clientes$Fecha_Nacimiento)/368.25,0))#Redondear, y especificar los decimales


############## ANALIZAR LAS PROPIEDADES DE LOS DATOS ################


# ?Cuantos usuarios hay ? ?Cual es el total de transacciones?   

nrow(clientes)
nrow(transacciones)

# ?Cual es el promedio de transacciones por usuario?

(nrow(transacciones)/nrow(clientes))

## Analicemos la edad ##

# Medidas de tendencia central

max(clientes$edad)
min(clientes$edad)

mean(clientes$edad)  
median(clientes$edad)

install.packages("modes") #Para calcular la moda es necesario 
library(modes)

modes(clientes$edad)

quantile(clientes$edad) # Dividen los datos y dicen cuantos datos hay por cada percentil
IQR(clientes$edad) # Diferencia entre el primer y el cuarto cuartil

summary(clientes$edad) # Tabla resumen
boxplot(clientes$edad) # Gr?fico de bigotes, identificar datos atipicos

# Medidas de dispersi?n

# 1.Rango

range(clientes$edad)
range(clientes$edad,na.rm=TRUE)

nclass.Sturges(clientes$edad) #Ayuda a identificar cuantos intervalos puedo tener seg?n el rango
seq(22,66,length = nclass.Sturges(clientes$edad))
seq(22,66,length = 4) # n-1 intervalos de clase este caso 4-1 intervalos

# 1.1 Creamos la variable con los intervalos

clientes$Segmento_edad<-
  as.character(cut(clientes$edad,breaks = c(22, 36, 51, 66),labels = c("Joven","Adulto","Mayor")))

# 2.Frecuencias

table(clientes$edad)
table(clientes$edad,useNA="ifany") #Para identificar los valores nulos
hist(clientes$edad) # ?Qu? significa este dato?

# 3.Desviaciones

var(clientes$edad)
sd(clientes$edad) # ?Qu? significa este dato?


## Ejercicio practico / Analicemos la variable "Valor_compra" ##

# Recuerden primero las medidas de tendencia central y luego las medidas de dispersi?n



# Medidas de tendencia central

max(transacciones$Valor_Compra)
min(transacciones$Valor_Compra)

mean(transacciones$Valor_Compra)  
median(transacciones$Valor_Compra)
modes(transacciones$Valor_Compra)

quantile(transacciones$Valor_Compra)
IQR(transacciones$Valor_Compra)

summary(transacciones$Valor_Compra) # Tabla resumen

# Medidas de dispersi?n

# 1.Rango

range(transacciones$Valor_Compra)
range(transacciones$Valor_Compra,na.rm=TRUE)

nclass.Sturges(transacciones$Valor_Compra) #Ayuda a identificar cuantos intervalos puedo tener seg?n el rango
seq(30,350,length = nclass.Sturges(transacciones$Valor_Compra))
seq(30,350,length = 6) # n-1 intervalos de clase este caso 4-1 intervalos

# 1.1 Cremos la variable con los intervalos

transacciones$intervalosValorCompra<-
  cut(transacciones$Valor_Compra,breaks=seq(30,350,length= 6),include.lowest=TRUE)

# 2.Frecuencias

table(transacciones$Valor_Compra)
table(transacciones$Valor_Compra,useNA="ifany") #Para identificar los valores nulos
hist(transacciones$Valor_Compra) # ?Qu? significa este dato?

# 3.Desviaciones

var(transacciones$Valor_Compra)
sd(transacciones$Valor_Compra) # ?Qu? significa este dato?

## Analizamos cuales son los productos que m?s se venden ##

ftable(transacciones$Producto)
ftable(transacciones["Producto"])


# Ejercicio |||||| Muestrenme un data set de los productos m?s vendidos en cantidad y en valor







Producto_mas_vendido <-
  group_by(transacciones, Producto)%>%
  summarise(cantidad = n(), valol = sum (Valor_Compra))



############### TRASFORMACI?N DEL CONJUNTO DE DATOS #######################


clientes$Ciudad_cuanti<-
  as.numeric(clientes$Ciudad) #creo una nueva variable/Convierte en calitativa en cuantitativa

ftable(clientes$Ciudad_cuant,clientes$Ciudad ) #Barranquilla = 1, Bogot? = 2, Bucaramanca =3, Cali = 4, Medell?n =5


clientes$media_saldo<-
  as.numeric(mean(clientes$Saldo))

clientes$desviaci?n_saldo<-
  as.numeric(abs(clientes$media_saldo-clientes$Saldo))

# ?Como sacar el valor que compro cada individuo y el total de compras? 

Variable_valor_compra <-
  group_by(transacciones,Id)%>%
  summarise(total_compras = n(), valor_total=sum(Valor_Compra))


transacciones$descipcion_producto<- 
  paste(transacciones$Producto,"_",transacciones$Color,"_",transacciones$Marca)#Para concatenar

# ?Como sacar el producto m?s comprado?

Variable_mas_comprado <-
  group_by(transacciones,Id, Producto)%>%
  summarise(producto_mas_comprado = n())%>%
  top_n(1)%>%
  distinct(Id, .keep_all = TRUE)#%>%
  #select(1:2)

colnames(Variable_mas_comprado)[2]<-
  "producto_mas_comprado"

# ?La marca m?s comprada?

Variable_mas_comprado_marca <-
  group_by(transacciones,Id, Marca)%>%
  summarise(marca_mas_comprada = n())%>%
  top_n(1)%>%
 distinct(Id, .keep_all = TRUE)#%>%
 # select(1:2)

colnames(Variable_mas_comprado_marca)[2]<-
  "marca_mas_comprada"

# ?El color m?s comprado?

Variable_mas_comprado_color <-
  group_by(transacciones,Id, Color)%>%
  summarise(color_mas_comprada = n())%>%
  top_n(1)%>%
  distinct(Id, .keep_all = TRUE)#%>%
  #select(1:2)

colnames(Variable_mas_comprado_color)[2]<-
  "color_mas_comprada"

# ?Cual seria la m?n y max de compra de un individuo?

transacciones$Fecha_Compra<-
  as.Date(transacciones$Fecha_Compra,"%d/%m/%Y")
class(transacciones$Fecha_Compra)

Variable_fecha_compra <-
  group_by(transacciones,Id)%>%
  summarise(min_fecha= min(Fecha_Compra, na.rm = TRUE), max_fecha= max(Fecha_Compra, na.rm = TRUE))

# Crucemos los datos existentes

clientes_final <-
  left_join(clientes,Variable_valor_compra, by = "Id")

clientes_final <-
  left_join(clientes_final,Variable_mas_comprado, by = "Id")

clientes_final <-
  left_join(clientes_final,Variable_mas_comprado_marca, by = "Id")

clientes_final <-
  left_join(clientes_final,Variable_mas_comprado_color, by = "Id")

clientes_final <-
  left_join(clientes_final,Variable_fecha_compra, by = "Id")

setwd("C:/Users/USUARIO/Desktop/Diplomado/Clase 4")

write.csv(datos_final, "datos_usuarios.csv")

###### K - Medias ######

#Normalizamos

clientes_final_nuevo<-
  group_by(clientes_final, Id, edad, Antig?edad, Saldo, valor_total, total_compras)%>%
  summarize()

clientes_final_scale<-
 as.data.frame(scale(clientes_final_nuevo[,2:6]))


#Creamos los clusters

set.seed(80)#Sembramos una semilla, esteria el componente aleatorio

clientes_final_KM <-
  kmeans(clientes_final_scale, centers = 4) # Numero de clusters que quiero
  names(clientes_final_KM)

clientes_final_KM$cluster # Asignaci?n de observaciones por cluster
clientes_final_KM$totss #Inercia Total
clientes_final_KM$betweenss #Inercia Intergrupos (mayor posible)
clientes_final_KM$withinss #Inercia Intragrupos
clientes_final_KM$tot.withinss #La suma de las intragrupos (menor posible)

#Determinemos el n?mero de clusters ?ptimo

  vector<-
    kmeans(clientes_final_scale, centers = 1)$betweenss #Almacene la frecuencia de la inercia intergrupos
  
  for(i in 2:20) vector[i]<-kmeans(clientes_final_scale, centers = i)$betweenss

  plot(1:20, vector, type = "b", xlab = "Numero de clusters", ylab = "suma de cuadrados intercluster")
  
# Inspecci?n de los resultados
  
  plot(clientes_final_nuevo$valor_total, clientes_final_nuevo$Antig?edad, col= clientes_final_KM$cluster, xlab= "Dinero", ylab= "Fidelidad")
aggregate(clientes_final_nuevo[,2:6], by=list(clientes_final_KM$cluster), mean)

plot(clientes_final_nuevo$valor_total, clientes_final_nuevo$Saldo, col= clientes_final_KM$cluster, xlab= "Dinero", ylab= "Fidelidad")


#Creaci?n de la variable

clientes_final_nuevo$cluster<-
  as.numeric(clientes_final_KM$cluster)

clientes_final_nuevo_2<-
 group_by(clientes_final_nuevo,Id, cluster)%>%
  summarise()

clientes_final <-
  left_join(clientes_final, clientes_final_nuevo_2,by= "Id")


##### Regresi?n lineal simple - Modelo supervisado #####

attach(clientes_final_nuevo)
pairs(clientes_final_nuevo[,2:6])
cor(edad, Saldo)
cor(Saldo, Antig?edad)
cor(total_compras, valor_total)

modelo<-
  lm(valor_total ~ total_compras)#Calculo del modelo

plot(total_compras, valor_total, xlab = "compras", ylab = "prueba regresi?n")
abline(modelo,col= "Blue")
summary(modelo)

#Predecir valores a traves del modelo

predict(modelo, data_frame(total_compras=16))#Calcula la ecuaci?n de la recta


predict(modelo, data_frame(total_compras=16), level = 0.95, interval = "prediction")#Calcula la ecuacion de la recta


clientes_final$valor_estimado<-
  as.numeric(modelo$fitted.values)
