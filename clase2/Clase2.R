##Ejercicio1    ###    Clase 2######

#####Base de Datos###
Usuario<- 
  read.csv("/Users/yamaha/Dev/Rstudio/clase2/Ejercicio1.csv", sep = ",", header = TRUE)

###librerias y paquetes
install.packages("dplyr")
library(stats)
library(dplyr)


#usuarios masculinos
usuario_masculino<-
  filter(Usuario, Genero=="masculino")

#usuarios consuman producto A & B
Personas_AyB<-
  filter(Usuario,Referencia_producto=="A" & Referencia_producto=="B")

#usuarios consuman producto A | B
Personas_AoB<-
  filter(Usuario,Referencia_producto=="A" | Referencia_producto=="B")

#Femenino diferente A
Femenino_diferenteA<-
  filter(Usuario,Genero == "femenino" & Referencia_producto != "A")

#Femenino diferente de B y que sea soltero
Femenino_DiferenteBySoltero<-
  filter(Usuario,Genero =="femenino" & Referencia_producto != "B" & Estado_civil == "soltero/a")

# Mayor a 70mil
Mayor_70mil<-
  filter(Usuario,Valor_Compra > 70000)

#Productos por ciudad que mas ventas obtubieron
Prod_vendidos<-
  group_by(Usuario, Referencia_producto) %>% 
  summarise(ventas=max(Valor_Compra))
maximo_vendido<-
  group_by(Prod_vendidos,Ciudad)%>%
  summarise(mayor_vendido=max(ventas))
  

#datos %>%
#  group_by(pais) %>%
#  select(sexo:pais) %>%
#  summarise(media.edad = mean(edad, na.rm = TRUE)) %>%
#  filter (media.edad > 25 & media.edad < 80)


Prod_vendidos2<-
  group_by(Usuario, Ciudad) %>% 
  summarise(ventas=first(Valor_Compra)) 
 # filter(first(Referencia_producto))


  


# cuanto es el numero de usuarios que se les puede eviar mail compras > 80mil

mail<-
  filter(Usuario, (Valor_Compra > 80000 & Acepta_envio_mail=="SI"))
str(mail)

#Cuántas mujeres de la ciudad de Medellín tienen una edad superior a los 30 años y 
#tienen un valor de compra mayor a 90 mil pesos

mujeres_medellin<-
  filter(Usuario, Ciudad=="Medellin" & Genero=="Femenino" & Edad > 30)

#if_else
#left_join
maximo_vendido %>% left_join(ventas)

