#primers paso en Rstudio

###### base de Datos/cargar archivo #######

clientes <- 
  read.csv("/Users/yamaha/Dev/Rstudio/clase1/clientes.csv", sep = ";", header = TRUE)


######paquetes y librerias#######
install.packages("dplyr")
library(stats)
library(dplyr)

# cuantos son nuestros clientes
nrow(clientes)

#consultas

str(clientes)
names(clientes)
summary(clientes)
# cual es el valor promedio de compra

mean(clientes$Valor_Compra)
mean(clientes$Saldo)

# cual es el valor de compra maxi y min

max(clientes$Valor_Compra)
min(clientes$Valor_Compra)

max(clientes$Saldo)
min(clientes$Saldo)

#cual es la desviacion estandar del valor de las compras

sd(clientes$Valor_Compra)
sd(clientes$Saldo)

#cuantos tipos de productos hay

ftable(clientes["Producto"])
ftable(clientes["Ciudad"])
ftable(clientes["Genero"])

#de forma grafica
plot (clientes$Producto, main = "Cantidad de productos comprados", 
      col = "dark blue", xlab = "licores", ylab = "Numero de productos")
plot(clientes$Ciudad, main = "Cantidad personas por ciudad", 
     col = "dark red", xlab = "ciudades", ylab = "Numero de Personas")

#cuantos clientes ahi por ciudad y genero
ftable(clientes[c("Ciudad", "Genero", "Producto")])
ftable(clientes[c("Ciudad", "Producto")])

ftable(clientes["Ciudad"])

########ACTIVIDAD#########

#Filtros

usuarios_saldo<-
  filter(clientes, Saldo > 3000)
















