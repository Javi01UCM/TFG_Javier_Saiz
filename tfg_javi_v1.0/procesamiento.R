# Cargo las librerias
library(gifski)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(sf)
library(tmap)
library(readxl)
library(forecast)
library(dplyr)
library(fUnitRoots)
library(lmtest)
library(zoo)
library(factoextra)

#Para limpiar los datos
rm(list=ls())

PEATONES_2019 <- read_excel("PEATONES/PEATONES_2019.xlsx")
View(PEATONES_2019)

PEATONES_2020 <- read_excel("PEATONES/PEATONES_2020.xlsx")
View(PEATONES_2020)

PEATONES_2021 <- read_excel("PEATONES/PEATONES_2021.xlsx")
View(PEATONES_2021)

#Cargar el mapa
getwd()
mapa <- st_read("Distritos/Distritos_20210712.shp")
tm_shape(mapa) +
  tm_borders() +
  tm_fill("NOMBRE", alpha=0.7) + 
  tm_layout(legend.outside = TRUE, title="Distrito") +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(type = "4star", position=c("right", "top")) 

unique(mapa$NOMBRE)
unique(PEATONES_2020$DISTRITO)

mapa <- subset(mapa, mapa$COD_DIS == 1 |
                 mapa$COD_DIS == 10 |
                 mapa$COD_DIS == 2 |
                 mapa$COD_DIS == 7 |
                 mapa$COD_DIS == 9)

#Cargar datasets
#En este tfg trabajaremos con 3 archivos xlsx de donde obtendremo slos datos de peatones y otros datos
#utiles como los distritos  o la fecha y hora a ala que se recogieron las variables. Estos archivos son 
#PEATONES_2019.xlsx, PEATONES_2020.xlsx y PEATONES_2021.xlsx.estos archivos los hemos obtenido del portal
#de datos abiertos de la Comunidad de Madrid al que se puede accceder con el siguiente enlace
#https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=695cd64d6f9b9610VgnVCM1000001d4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default
#En import, opcion "From Excel"  importamos los datasets.

#Una vez tenemos cargados los datasets pasamos al mismo formato todos los datos de los 3 dataset

PEATONES_2019$FECHA<-as.Date(PEATONES_2019$FECHA, "%d-%m-%y")
PEATONES_2019$HORA<-as.character(PEATONES_2019$HORA)

PEATONES_2021$FECHA<-as.Date(PEATONES_2021$FECHA, "%d-%m-%y")
PEATONES_2021$HORA<-as.character(PEATONES_2021$HORA)

PEATONES_2020$FECHA<-as.Date(PEATONES_2020$FECHA, "%d-%m-%y")
PEATONES_2020$HORA<-format(as.POSIXct(PEATONES_2020$HORA), format = "%H:%M")
PEATONES_2020$HORA<-as.character(PEATONES_2020$HORA)

#-----------------------PEATONES TOTAL-----------------------------
#Unimos los datos de PEATONES_2019, PEATONES_2020 Y PEATONES_2021
PEATONES_TOTAL <- rbind(PEATONES_2019,PEATONES_2020)
PEATONES_TOTAL <- rbind(PEATONES_TOTAL,PEATONES_2021)

#Muestro un resumen general sobre todas las variables del dataframe
summary(PEATONES_TOTAL$PEATONES)

#Peatones por año
ggplot(data=PEATONES_TOTAL)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color = as.factor(year(FECHA))))+
  labs(color = "Año")

#Peatones contabilizados por distrito

ggplot(PEATONES_TOTAL, aes(x=DISTRITO, y=PEATONES)) +
  geom_jitter(aes(color = DISTRITO )) +
  geom_boxplot(aes(x=DISTRITO, y=PEATONES) ) + 
  coord_flip() + 
  labs(x = "Distritos", y = "Peatones contabilizados", 
       title ="Peatones  por distrito")

#Peatones en cada año separado por meses

ggplot(data=PEATONES_2019)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color=months(FECHA)))

ggplot(data=PEATONES_2020)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color=months(FECHA)))

ggplot(data=PEATONES_2021)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color=months(FECHA)))

 #----------------------------Analisis de 2019----------------------------------------
# Comprobamos que la dimension de la tabla de datos de peatones es 55632 x 12
dim(PEATONES_2019)
# Comprueba que los distritos que contiene PEATONES_2019 son: "Centro", "Chamberí",
#"Latina", "Moncloa-Aravaca", "Arganzuela".
unique(PEATONES_2019$DISTRITO)
# Comprueba que los codigos postales que contiene PEATONES_2019 son: 28004 28015 28014
#28012 28013 28001 28011 28008 28005.
unique(PEATONES_2019$CÓDIGO_POSTAL)

summary(PEATONES_2019$PEATONES)

#Obtenemos la fecha en tres columnas distintas, una para el año, otra para el mes y otra para el dia de la semana
#a la que corresponde esa fecha (puede sernos util mas adelante)
nuevo_dia <- weekdays(as.Date(PEATONES_2019$FECHA))
nuevo_dia
nuevo_mes <- format(PEATONES_2019$FECHA, "%B")
nuevo_mes
nuevo_año <- format(PEATONES_2019$FECHA, "%Y")
nuevo_año
p_2019 <- cbind(nuevo_año,nuevo_mes,nuevo_dia,PEATONES_2019)
p_2019

#Hacemos una nueva tabla que es el subconjunto de PEATONES_2019 con mas de 15000 peatones
mas_15000_peatones <- subset(PEATONES_2019, PEATONES_2019$PEATONES >= 15000 )
#La tabla mas_15000_peatones tiene mas  de 72 observaciones asi que cogemos las 18 observacion que mas
#peatones tengan
P_ordenados <- p_2019[order(p_2019$PEATONES, decreasing = T), ]
P_18_primeros <- P_ordenados[1:18, ]
unique(P_18_primeros$FECHA)
unique(P_18_primeros$DISTRITO)
unique(P_18_primeros$nuevo_dia)

#Hago una tabla con las observaciones que tienen 0 peatones
cero_peatones <- subset(PEATONES_2019, PEATONES_2019$PEATONES == 0 )

#Numero de peatones por dia de la semana
ggplot(data=p_2019)+
  geom_point(mapping=aes(x=nuevo_dia, y=PEATONES, color = nuevo_dia))+
  labs(color = "Dia de la semana")

#Numero de peatones por mes
ggplot(data=p_2019)+
  geom_point(mapping=aes(x=nuevo_mes, y=PEATONES, color = nuevo_mes))+
  labs(color = "Mes")

#Observaciones de la tabla PEATONES_2019 con mas de 50000 observaciones
m <- subset(p_2019, p_2019$PEATONES >= 50000 )

#Le cambio de nombre a CODIGO_POSTAL para quitar la tilde
names(PEATONES_2019)[9]<-"CODIGO_POSTAL"
#Comporbamos que se ha cambiado correctamente
str(PEATONES_2019)
#Convierto a factor el codigo postal
PEATONES_2019$CODIGO_POSTAL<-as.factor(PEATONES_2019$CODIGO_POSTAL)

#Creo un hiostograma que clasifique a los peatones por codigo postal
ggplot(PEATONES_2019) +
  geom_histogram(aes(x = PEATONES, fill = CODIGO_POSTAL), 
                 binwidth = 50) +
  coord_cartesian(xlim = c(0,100))

#Sacamos la media de peatones en las calles peatonales
cp1<-mean(select(filter(PEATONES_2019, OBSERVACIONES_DIRECCION=="Calle peatonal"),PEATONES)$PEATONES, na.rm = T)
cp1

#Sacamos la media de peatones en las aceras pares
cp2<-mean(select(filter(PEATONES_2019, OBSERVACIONES_DIRECCION=="Acera Pares"),PEATONES)$PEATONES, na.rm = T)
cp2

#Sacamos la media de peatones en las aceras impares
cp3<-mean(select(filter(PEATONES_2019, OBSERVACIONES_DIRECCION=="Acera Impares"),PEATONES)$PEATONES, na.rm = T)
cp3

#Peatones por fecha y separados por distrito

ggplot(PEATONES_2019) +
  geom_point(aes(x = FECHA, y = PEATONES, color = DISTRITO)) +
  facet_wrap(  ~ DISTRITO  ) +
  geom_smooth(aes(x = FECHA, y = PEATONES)) +
  labs( x = "Fecha", y = "Peatones contabilizados",
        title = "Peatones por fecha y separados por distrito - facet_wrap + geom_smooth")

#Peatones por distrito
ggplot(PEATONES_2019, aes(x=DISTRITO, y=PEATONES)) +
  geom_jitter(aes(color = DISTRITO )) +
  geom_boxplot(aes(x=DISTRITO, y=PEATONES) ) + 
  coord_flip() + 
  labs(x = "Distritos", y = "Peatones contabilizados", 
       title ="Peatones  por distrito - geom_jitter + coord_flip")

#mapa 2019

# Para añadir a la tabla 'mapa' la información de los peatones, 
# Creo un vector que luego con la suma total de peatones de cada distrito
# Finalmente, este nuevo vector se 'pega' al 'mapa' con  (cbind)

PeatxDistr_2019 <- c(1:5)

PeatxDistr_2019[1] <- sum((PEATONES_2019[PEATONES_2019$NÚMERO_DISTRITO == 1,])$PEATONES)
PeatxDistr_2019[2] <- sum((PEATONES_2019[PEATONES_2019$NÚMERO_DISTRITO == 10,])$PEATONES)
PeatxDistr_2019[3] <- sum((PEATONES_2019[PEATONES_2019$NÚMERO_DISTRITO == 2,])$PEATONES)
PeatxDistr_2019[4] <- sum((PEATONES_2019[PEATONES_2019$NÚMERO_DISTRITO == 7,])$PEATONES)
PeatxDistr_2019[5] <- sum((PEATONES_2019[PEATONES_2019$NÚMERO_DISTRITO == 9,])$PEATONES)

mapa_2019 <- cbind(mapa, PeatxDistr_2019)
mapa_2019

tm_shape(mapa_2019) +
  tm_borders() +
  tm_fill("PeatxDistr_2019") + 
  tm_layout(legend.outside = TRUE, title="Peatones por distrito") +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(type = "4star", position=c("right", "top")) 

# El distrito Centro tiene muchos peatones, esto impide que el resto
# de los distritos se diferencien entre ellos
# aplicamos escala logarítmica 

PeatxDistr_log_2019 <- log(PeatxDistr_2019,10)
PeatxDistr_log_2019

mapa_log_2019 <- cbind(mapa_2019, PeatxDistr_log_2019)

mapa_2019 <- cbind(mapa, PeatxDistr_log_2019)

tm_shape(mapa_2019) +
  tm_fill("PeatxDistr_log_2019", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito 2019",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))    # flecha indicando el norte

#GIF 2019
inicio <- as.Date("2019-09-01")
final <- as.Date("2019-12-31")
fecha <- inicio

PeatxDistr <- c(1:5)
PeatxDistr[1] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 2)&(PEATONES_2019$FECHA==fecha),])$PEATONES)
PeatxDistr[2] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 10)&(PEATONES_2019$FECHA==fecha),])$PEATONES)
PeatxDistr[3] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 1)&(PEATONES_2019$FECHA==fecha),])$PEATONES)
PeatxDistr[4] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 7)&(PEATONES_2019$FECHA==fecha),])$PEATONES)
PeatxDistr[5] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 9)&(PEATONES_2019$FECHA==fecha),])$PEATONES)

PeatxDistr <- log(PeatxDistr,10)

mapa_final <- cbind(mapa, PeatxDistr, rep(fecha,5))

fecha <- fecha + 1

while(fecha <= final){
  
  PeatxDistr[1] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 2)&(PEATONES_2019$FECHA==fecha),])$PEATONES)
  PeatxDistr[2] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 10)&(PEATONES_2019$FECHA==fecha),])$PEATONES)
  PeatxDistr[3] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 1)&(PEATONES_2019$FECHA==fecha),])$PEATONES)
  PeatxDistr[4] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 7)&(PEATONES_2019$FECHA==fecha),])$PEATONES)
  PeatxDistr[5] <- sum((PEATONES_2019[(PEATONES_2019$NÚMERO_DISTRITO == 9)&(PEATONES_2019$FECHA==fecha),])$PEATONES)
  
  PeatxDistr <- log(PeatxDistr,10)
  
  mapa_unir_cols <- cbind(mapa, PeatxDistr, rep(fecha,5))
  
  mapa_final <- rbind(mapa_final, mapa_unir_cols)
  
  fecha <- fecha+1
}

names(mapa_final)[8] <- "fecha"

m1 <- tm_shape(mapa_final) +
  tm_fill("PeatxDistr", title="Peatones (logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, title="Peatones por distrito 2019") +
  tm_text("DISTRI_MAY", size=0.5, ymod=0.35) +
  tm_compass(position=c("right", "top")) +
  tm_scale_bar(position=c("right", "bottom")) +
  tm_facets(along="fecha") 


tmap_animation(m1, delay=20)

tmap_animation(m1, filename = "GIFs/PeatonesxDistrito2019.gif", delay=20)

#----------------------------------Analisis de 2020---------------------------------
# Comprobamos que la dimension de la tabla de datos de peatones es 166896 x 12
dim(PEATONES_2020)
# Comprueba que los distritos que contiene PEATONES_2019 son: "Centro", "Chamberí",
#"Latina", "Moncloa-Aravaca", "Arganzuela".
unique(PEATONES_2020$DISTRITO)
# Comprueba que los codigos postales que contiene PEATONES_2019 son: 28004 28015 28014
#28012 28013 28001 28011 28008 28005.
unique(PEATONES_2020$CÓDIGO_POSTAL)

summary(PEATONES_2020$PEATONES)

#Le cambio de nombre a CODIGO_POSTAL para quitar la tilde
names(PEATONES_2020)[9]<-"CODIGO_POSTAL"
#Comporbamos que se ha cambiado correctamente
str(PEATONES_2020)
#Convierto a factor el codigo postal
PEATONES_2020$CODIGO_POSTAL<-as.factor(PEATONES_2020$CODIGO_POSTAL)

#Creo un hiostograma que clasifique a los peatones por codigo postal
ggplot(PEATONES_2020) +
  geom_histogram(aes(x = PEATONES, fill = CODIGO_POSTAL), 
                 binwidth = 50) +
  coord_cartesian(xlim = c(0,100))

#creo una tabla para cada fase por la que paso el año 2020: prepandemia, confinamiento, estabilizacio
#y segunda ola

prepandemia <- filter(PEATONES_2020, between(FECHA, as.Date('2020-01-01'), as.Date('2020-03-15')))
confinamiento <- filter(PEATONES_2020, between(FECHA, as.Date('2020-03-16'), as.Date('2020-05-01')))
estabilizacion <- filter(PEATONES_2020, between(FECHA, as.Date('2020-05-02'), as.Date('2020-09-04')))
segunda_ola <- filter(PEATONES_2020, between(FECHA, as.Date('2020-09-05'), as.Date('2020-12-31')))

#Crea una nueva columna en la tabla PEATONES_2020  a la cual llamo FASE y la divido en las cuatro fases
#anteriormente mencionadas para tener localizada cada observacion con el periodo por el que paso en cada fecha
PEATONES_2020$FASE <- ifelse(PEATONES_2020$FECHA <= as.Date('2020-03-15'), 
                             "prepandemia",
                             ifelse(PEATONES_2020$FECHA <= as.Date('2020-05-01'),
                                    "confinamiento",
                                    ifelse(PEATONES_2020$FECHA <= as.Date('2020-09-04'), 
                                           "estabilizacion",
                                           "segunda_ola")))

#Numero de peatones por fase
ggplot(data=PEATONES_2020)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color=FASE))

#Muestro un resumen general sobre todas las variables del dataframe PEATONES_2020 y las tablas
#creadas anteriormente por fase
summary(PEATONES_2020$PEATONES)

summary(prepandemia$PEATONES)
summary(confinamiento$PEATONES)
summary(estabilizacion$PEATONES)
summary(segunda_ola$PEATONES)

#Peatones contabilizados por distrito
ggplot(PEATONES_2020, aes(x=DISTRITO, y=PEATONES)) +
  geom_jitter(aes(color = DISTRITO )) +
  geom_boxplot(aes(x=DISTRITO, y=PEATONES) ) + 
  coord_flip() + 
  labs(x = "Distritos", y = "Peatones contabilizados", 
       title ="Peatones  por distrito - geom_jitter + coord_flip")

#Peatones contabilizados por fase
ggplot(PEATONES_2020, aes(x=FASE, y=PEATONES)) +
  geom_jitter(aes(color = FASE )) +
  geom_boxplot(aes(x=FASE, y=PEATONES) ) + 
  coord_flip() + 
  labs(x = "Distritos", y = "Peatones contabilizados", 
       title ="Peatones  por fase - geom_jitter + coord_flip")

#Peatones contabilizados por fecha y separados por distrito
ggplot(PEATONES_2020) +
  geom_point(aes(x = FECHA, y = PEATONES, color = DISTRITO)) +
  facet_wrap(  ~ DISTRITO  ) +
  geom_smooth(aes(x = FECHA, y = PEATONES)) +
  labs( x = "Fecha", y = "Peatones contabilizados",
        title = "Peatones por fecha y separados por distrito - facet_wrap + geom_smooth")

#mapa 2020

PeatxDistr_2020 <- c(1:5)

PeatxDistr_2020[1] <- sum((PEATONES_2020[PEATONES_2020$NÚMERO_DISTRITO == 1,])$PEATONES)
PeatxDistr_2020[2] <- sum((PEATONES_2020[PEATONES_2020$NÚMERO_DISTRITO == 10,])$PEATONES)
PeatxDistr_2020[3] <- sum((PEATONES_2020[PEATONES_2020$NÚMERO_DISTRITO == 2,])$PEATONES)
PeatxDistr_2020[4] <- sum((PEATONES_2020[PEATONES_2020$NÚMERO_DISTRITO == 7,])$PEATONES)
PeatxDistr_2020[5] <- sum((PEATONES_2020[PEATONES_2020$NÚMERO_DISTRITO == 9,])$PEATONES)

mapa_2020 <- cbind(mapa, PeatxDistr_2020)
mapa_2020

tm_shape(mapa_2020) +
  tm_borders() +
  tm_fill("PeatxDistr_2020") + 
  tm_layout(legend.outside = TRUE, title="Peatones por distrito 2020") +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(type = "4star", position=c("right", "top")) 

PeatxDistr_log_2020 <- log(PeatxDistr_2020,10)
PeatxDistr_log_2020

mapa_log_2020 <- cbind(mapa_2020, PeatxDistr_log_2020)

mapa_2020 <- cbind(mapa, PeatxDistr_log_2020)

tm_shape(mapa_2020) +
  tm_fill("PeatxDistr_log_2020", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito 2020",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))    # flecha indicando el norte

#mapa prepandemia

PeatxDistr_prepandemia <- c(1:5)

PeatxDistr_prepandemia[1] <- sum((prepandemia[prepandemia$NÚMERO_DISTRITO == 1,])$PEATONES)
PeatxDistr_prepandemia[2] <- sum((prepandemia[prepandemia$NÚMERO_DISTRITO == 10,])$PEATONES)
PeatxDistr_prepandemia[3] <- sum((prepandemia[prepandemia$NÚMERO_DISTRITO == 2,])$PEATONES)
PeatxDistr_prepandemia[4] <- sum((prepandemia[prepandemia$NÚMERO_DISTRITO == 7,])$PEATONES)
PeatxDistr_prepandemia[5] <- sum((prepandemia[prepandemia$NÚMERO_DISTRITO == 9,])$PEATONES)

mapa_prepandemia <- cbind(mapa, PeatxDistr_prepandemia)
mapa_prepandemia

tm_shape(mapa_prepandemia) +
  tm_borders() +
  tm_fill("PeatxDistr_prepandemia") + 
  tm_layout(legend.outside = TRUE, title="Peatones por distrito prepandemia") +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(type = "4star", position=c("right", "top")) 

PeatxDistr_log_prepandemia <- log(PeatxDistr_prepandemia,10)
PeatxDistr_log_prepandemia

mapa_log_prepandemia <- cbind(mapa_prepandemia, PeatxDistr_log_prepandemia)

mapa_prepandemia <- cbind(mapa, PeatxDistr_log_prepandemia)

tm_shape(mapa_prepandemia) +
  tm_fill("PeatxDistr_log_prepandemia", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito prepandemia",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))    # flecha indicando el norte

#mapa confinamiento

PeatxDistr_confinamiento <- c(1:5)

PeatxDistr_confinamiento[1] <- sum((confinamiento[confinamiento$NÚMERO_DISTRITO == 1,])$PEATONES)
PeatxDistr_confinamiento[2] <- sum((confinamiento[confinamiento$NÚMERO_DISTRITO == 10,])$PEATONES)
PeatxDistr_confinamiento[3] <- sum((confinamiento[confinamiento$NÚMERO_DISTRITO == 2,])$PEATONES)
PeatxDistr_confinamiento[4] <- sum((confinamiento[confinamiento$NÚMERO_DISTRITO == 7,])$PEATONES)
PeatxDistr_confinamiento[5] <- sum((confinamiento[confinamiento$NÚMERO_DISTRITO == 9,])$PEATONES)

mapa_confinamiento <- cbind(mapa, PeatxDistr_confinamiento)
mapa_confinamiento

tm_shape(mapa_confinamiento) +
  tm_borders() +
  tm_fill("PeatxDistr_confinamiento") + 
  tm_layout(legend.outside = TRUE, title="Peatones por distrito confinamiento") +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(type = "4star", position=c("right", "top")) 

PeatxDistr_log_confinamiento <- log(PeatxDistr_confinamiento,10)
PeatxDistr_log_confinamiento

mapa_log_confinamiento <- cbind(mapa_confinamiento, PeatxDistr_log_confinamiento)

mapa_confinamiento <- cbind(mapa, PeatxDistr_log_confinamiento)

tm_shape(mapa_confinamiento) +
  tm_fill("PeatxDistr_log_confinamiento", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito confinamiento",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))    # flecha indicando el norte

#mapa estabilizacion

PeatxDistr_estabilizacion <- c(1:5)

PeatxDistr_estabilizacion[1] <- sum((estabilizacion[estabilizacion$NÚMERO_DISTRITO == 1,])$PEATONES)
PeatxDistr_estabilizacion[2] <- sum((estabilizacion[estabilizacion$NÚMERO_DISTRITO == 10,])$PEATONES)
PeatxDistr_estabilizacion[3] <- sum((estabilizacion[estabilizacion$NÚMERO_DISTRITO == 2,])$PEATONES)
PeatxDistr_estabilizacion[4] <- sum((estabilizacion[estabilizacion$NÚMERO_DISTRITO == 7,])$PEATONES)
PeatxDistr_estabilizacion[5] <- sum((estabilizacion[estabilizacion$NÚMERO_DISTRITO == 9,])$PEATONES)

mapa_estabilizacion <- cbind(mapa, PeatxDistr_estabilizacion)
mapa_estabilizacion

tm_shape(mapa_estabilizacion) +
  tm_borders() +
  tm_fill("PeatxDistr_estabilizacion") + 
  tm_layout(legend.outside = TRUE, title="Peatones por distrito estabilizacion") +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(type = "4star", position=c("right", "top")) 

PeatxDistr_log_estabilizacion <- log(PeatxDistr_estabilizacion,10)
PeatxDistr_log_estabilizacion

mapa_log_estabilizacion <- cbind(mapa_estabilizacion, PeatxDistr_log_estabilizacion)

mapa_estabilizacion <- cbind(mapa, PeatxDistr_log_estabilizacion)

tm_shape(mapa_estabilizacion) +
  tm_fill("PeatxDistr_log_estabilizacion", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito estabilizacion",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))    # flecha indicando el norte

#mapa segunda_ola

PeatxDistr_segunda_ola <- c(1:5)

PeatxDistr_segunda_ola[1] <- sum((segunda_ola[segunda_ola$NÚMERO_DISTRITO == 1,])$PEATONES)
PeatxDistr_segunda_ola[2] <- sum((segunda_ola[segunda_ola$NÚMERO_DISTRITO == 10,])$PEATONES)
PeatxDistr_segunda_ola[3] <- sum((segunda_ola[segunda_ola$NÚMERO_DISTRITO == 2,])$PEATONES)
PeatxDistr_segunda_ola[4] <- sum((segunda_ola[segunda_ola$NÚMERO_DISTRITO == 7,])$PEATONES)
PeatxDistr_segunda_ola[5] <- sum((segunda_ola[segunda_ola$NÚMERO_DISTRITO == 9,])$PEATONES)

mapa_segunda_ola <- cbind(mapa, PeatxDistr_segunda_ola)
mapa_segunda_ola

tm_shape(mapa_segunda_ola) +
  tm_borders() +
  tm_fill("PeatxDistr_segunda_ola") + 
  tm_layout(legend.outside = TRUE, title="Peatones por distrito segunda_ola") +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(type = "4star", position=c("right", "top")) 

PeatxDistr_log_segunda_ola <- log(PeatxDistr_segunda_ola,10)
PeatxDistr_log_segunda_ola

mapa_log_segunda_ola <- cbind(mapa_segunda_ola, PeatxDistr_log_segunda_ola)

mapa_segunda_ola <- cbind(mapa, PeatxDistr_log_segunda_ola)

tm_shape(mapa_segunda_ola) +
  tm_fill("PeatxDistr_log_segunda_ola", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito segunda_ola",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))    # flecha indicando el norte

#GIF 2020
inicio <- as.Date("2020-01-01")
final <- as.Date("2020-12-31")
fecha <- inicio

PeatxDistr <- c(1:5)
PeatxDistr[1] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 2)&(PEATONES_2020$FECHA==fecha),])$PEATONES)
PeatxDistr[2] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 10)&(PEATONES_2020$FECHA==fecha),])$PEATONES)
PeatxDistr[3] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 1)&(PEATONES_2020$FECHA==fecha),])$PEATONES)
PeatxDistr[4] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 7)&(PEATONES_2020$FECHA==fecha),])$PEATONES)
PeatxDistr[5] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 9)&(PEATONES_2020$FECHA==fecha),])$PEATONES)

PeatxDistr <- log(PeatxDistr,10)

mapa_final <- cbind(mapa, PeatxDistr, rep(fecha,5))

fecha <- fecha + 1

while(fecha <= final){
  
  PeatxDistr[1] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 2)&(PEATONES_2020$FECHA==fecha),])$PEATONES)
  PeatxDistr[2] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 10)&(PEATONES_2020$FECHA==fecha),])$PEATONES)
  PeatxDistr[3] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 1)&(PEATONES_2020$FECHA==fecha),])$PEATONES)
  PeatxDistr[4] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 7)&(PEATONES_2020$FECHA==fecha),])$PEATONES)
  PeatxDistr[5] <- sum((PEATONES_2020[(PEATONES_2020$NÚMERO_DISTRITO == 9)&(PEATONES_2020$FECHA==fecha),])$PEATONES)
  
  PeatxDistr <- log(PeatxDistr,10)
  
  mapa_unir_cols <- cbind(mapa, PeatxDistr, rep(fecha,5))
  
  mapa_final <- rbind(mapa_final, mapa_unir_cols)
  
  fecha <- fecha+1
}

names(mapa_final)[8] <- "fecha"

m1 <- tm_shape(mapa_final) +
  tm_fill("PeatxDistr", title="Peatones (logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, title="Peatones por distrito 2020") +
  tm_text("DISTRI_MAY", size=0.5, ymod=0.35) +
  tm_compass(position=c("right", "top")) +
  tm_scale_bar(position=c("right", "bottom")) +
  tm_facets(along="fecha") 


tmap_animation(m1, delay=20)

tmap_animation(m1, filename = "GIFs/PeatonesxDistrito2020.gif", delay=20)

#------------------------------------Analisis 2021---------------------------------

# Comprobamos que la dimension de la tabla de datos de peatones es 82536 x 12
dim(PEATONES_2021)
# Comprueba que los distritos que contiene PEATONES_2019 son: "Centro", "Chamberí",
#"Latina", "Moncloa-Aravaca", "Arganzuela".
unique(PEATONES_2021$DISTRITO)
# Comprueba que los codigos postales que contiene PEATONES_2019 son: 28004 28015 28014
#28012 28013 28001 28011 28008 28005.
unique(PEATONES_2021$CÓDIGO_POSTAL)

summary(PEATONES_2021$PEATONES)

#Le cambio de nombre a CODIGO_POSTAL para quitar la tilde
names(PEATONES_2021)[9]<-"CODIGO_POSTAL"
#Comporbamos que se ha cambiado correctamente
str(PEATONES_2021)
#Convierto a factor el codigo postal
PEATONES_2021$CODIGO_POSTAL<-as.factor(PEATONES_2021$CODIGO_POSTAL)

#Creo un hiostograma que clasifique a los peatones por codigo postal
ggplot(PEATONES_2021) +
  geom_histogram(aes(x = PEATONES, fill = CODIGO_POSTAL), 
                 binwidth = 50) +
  coord_cartesian(xlim = c(0,100))

#Hago una tabla  para los eventos de Filomena y la tercera ola
peatones_2021_filomena <- filter(PEATONES_2021, between(FECHA, as.Date('2021-01-08'), as.Date('2021-01-15')))
tercera_ola <- filter(PEATONES_2021, between(FECHA, as.Date('2021-01-01'), as.Date('2021-02-15')))

summary(peatones_2021_filomena$PEATONES)
summary(tercera_ola$PEATONES)

#Peatones contabilizados por distrito
ggplot(PEATONES_2021, aes(x=DISTRITO, y=PEATONES)) +
  geom_jitter(aes(color = DISTRITO )) +
  geom_boxplot(aes(x=DISTRITO, y=PEATONES) ) + 
  coord_flip() + 
  labs(x = "Distritos", y = "Peatones contabilizados", 
       title ="Peatones  por distrito - geom_jitter + coord_flip")

#Peatones contabilizados por fecha y separados por distrito
ggplot(PEATONES_2021) +
  geom_point(aes(x = FECHA, y = PEATONES, color = DISTRITO)) +
  facet_wrap(  ~ DISTRITO  ) +
  geom_smooth(aes(x = FECHA, y = PEATONES)) +
  labs( x = "Fecha", y = "Peatones contabilizados",
        title = "Peatones por fecha y separados por distrito - facet_wrap + geom_smooth")

ggplot(data=PEATONES_2021)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color=months(FECHA)))

#mapa 2021

PeatxDistr_2021 <- c(1:5)

PeatxDistr_2021[1] <- sum((PEATONES_2021[PEATONES_2021$NÚMERO_DISTRITO == 1,])$PEATONES)
PeatxDistr_2021[2] <- sum((PEATONES_2021[PEATONES_2021$NÚMERO_DISTRITO == 10,])$PEATONES)
PeatxDistr_2021[3] <- sum((PEATONES_2021[PEATONES_2021$NÚMERO_DISTRITO == 2,])$PEATONES)
PeatxDistr_2021[4] <- sum((PEATONES_2021[PEATONES_2021$NÚMERO_DISTRITO == 7,])$PEATONES)
PeatxDistr_2021[5] <- sum((PEATONES_2021[PEATONES_2021$NÚMERO_DISTRITO == 9,])$PEATONES)

mapa_2021 <- cbind(mapa, PeatxDistr_2021)
mapa_2021

tm_shape(mapa_2021) +
  tm_borders() +
  tm_fill("PeatxDistr_2021") + 
  tm_layout(legend.outside = TRUE, title="Peatones por distrito 2021") +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(type = "4star", position=c("right", "top")) 

PeatxDistr_log_2021 <- log(PeatxDistr_2021,10)
PeatxDistr_log_2021

mapa_log_2021 <- cbind(mapa_2021, PeatxDistr_log_2021)

mapa_2021 <- cbind(mapa, PeatxDistr_log_2021)

tm_shape(mapa_2021) +
  tm_fill("PeatxDistr_log_2021", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito 2021",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))    # flecha indicando el norte

#GIF 2021
inicio <- as.Date("2021-01-01")
final <- as.Date("2021-06-30")
fecha <- inicio

PeatxDistr <- c(1:5)
PeatxDistr[1] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 2)&(PEATONES_2021$FECHA==fecha),])$PEATONES)
PeatxDistr[2] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 10)&(PEATONES_2021$FECHA==fecha),])$PEATONES)
PeatxDistr[3] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 1)&(PEATONES_2021$FECHA==fecha),])$PEATONES)
PeatxDistr[4] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 7)&(PEATONES_2021$FECHA==fecha),])$PEATONES)
PeatxDistr[5] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 9)&(PEATONES_2021$FECHA==fecha),])$PEATONES)

PeatxDistr <- log(PeatxDistr,10)

mapa_final <- cbind(mapa, PeatxDistr, rep(fecha,5))

fecha <- fecha + 1

while(fecha <= final){
  
  PeatxDistr[1] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 2)&(PEATONES_2021$FECHA==fecha),])$PEATONES)
  PeatxDistr[2] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 10)&(PEATONES_2021$FECHA==fecha),])$PEATONES)
  PeatxDistr[3] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 1)&(PEATONES_2021$FECHA==fecha),])$PEATONES)
  PeatxDistr[4] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 7)&(PEATONES_2021$FECHA==fecha),])$PEATONES)
  PeatxDistr[5] <- sum((PEATONES_2021[(PEATONES_2021$NÚMERO_DISTRITO == 9)&(PEATONES_2021$FECHA==fecha),])$PEATONES)
  
  PeatxDistr <- log(PeatxDistr,10)
  
  mapa_unir_cols <- cbind(mapa, PeatxDistr, rep(fecha,5))
  
  mapa_final <- rbind(mapa_final, mapa_unir_cols)
  
  fecha <- fecha+1
}

names(mapa_final)[8] <- "fecha"

m1 <- tm_shape(mapa_final) +
  tm_fill("PeatxDistr", title="Peatones (logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, title="Peatones por distrito 2021") +
  tm_text("DISTRI_MAY", size=0.5, ymod=0.35) +
  tm_compass(position=c("right", "top")) +
  tm_scale_bar(position=c("right", "bottom")) +
  tm_facets(along="fecha") 


tmap_animation(m1, delay=20)

tmap_animation(m1, filename = "GIFs/PeatonesxDistrito2021.gif", delay=20)


#----------------------------------------------------------------------------------------
#Paso a pdf todas las graficas y imagenes realizadas durante el estudio

p <- ggplot(data=PEATONES_2019)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color=months(FECHA)))

pdf(file = "PDFs/peatones_2019_por_mes.pdf")

p

dev.off()

p<-ggplot(data=PEATONES_TOTAL)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color = as.factor(year(FECHA))))+
  labs(color = "Año")
pdf(file = "PDFs/peatones_total_año_grafica.pdf")
p
dev.off()

p <- ggplot(PEATONES_TOTAL, aes(x=DISTRITO, y=PEATONES)) +
  geom_jitter(aes(color = DISTRITO )) +
  geom_boxplot(aes(x=DISTRITO, y=PEATONES) ) + 
  coord_flip() + 
  labs(x = "Distritos", y = "Peatones contabilizados", 
       title ="Peatones  por distrito")
pdf(file = "PDFs/peatones_contabilizados_por_distrito_total.pdf")
p
dev.off()

p <- ggplot(data=p_2019)+
  geom_point(mapping=aes(x=nuevo_dia, y=PEATONES, color = nuevo_dia))+
  labs(color = "Dia de la semana")
pdf(file = "PDFs/numero_peatones_por_dia_de_la_semana_2019.pdf")
p
dev.off()

p <- ggplot(data=p_2019)+
  geom_point(mapping=aes(x=nuevo_mes, y=PEATONES, color = nuevo_mes))+
  labs(color = "Mes")
pdf(file = "PDFs/numero_de_peatones_por_mes_2019.pdf")
p
dev.off()

p <- ggplot(PEATONES_2019) +
  geom_point(aes(x = FECHA, y = PEATONES, color = DISTRITO)) +
  facet_wrap(  ~ DISTRITO  ) +
  geom_smooth(aes(x = FECHA, y = PEATONES)) +
  labs( x = "Fecha", y = "Peatones contabilizados",
        title = "Peatones por fecha y separados por distrito - facet_wrap + geom_smooth")
pdf(file = "PDFs/peatones_por_fecha_y_separados_por_distrito_2019.pdf")
p
dev.off()

p <- tm_shape(mapa_2019) +
  tm_fill("PeatxDistr_log_2019", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito 2019",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) + 
  tm_compass(position=c("right", "top"))
pdf(file = "PDFs/mapa_peatones_log_2019.pdf")
p
dev.off()

p <- ggplot(PEATONES_2019, aes(x=DISTRITO, y=PEATONES)) +
  geom_jitter(aes(color = DISTRITO )) +
  geom_boxplot(aes(x=DISTRITO, y=PEATONES) ) + 
  coord_flip() + 
  labs(x = "Distritos", y = "Peatones contabilizados", 
       title ="Peatones  por distrito - geom_jitter + coord_flip")
pdf(file = "PDFs/peatones_por_distrito_2019.pdf")
p
dev.off()

p <- #Numero de peatones por fase
  ggplot(data=PEATONES_2020)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color=FASE))
  pdf(file = "PDFs/peatones_por_fase_2020.pdf")
p
dev.off()


p <- #Numero de peatones por fase
  ggplot(data=PEATONES_2020)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color=FASE))
pdf(file = "PDFs/peatones_por_fase_2020.pdf")
p
dev.off()

p <- ggplot(PEATONES_2020, aes(x=DISTRITO, y=PEATONES)) +
  geom_jitter(aes(color = DISTRITO )) +
  geom_boxplot(aes(x=DISTRITO, y=PEATONES) ) + 
  coord_flip() + 
  labs(x = "Distritos", y = "Peatones contabilizados", 
       title ="Peatones  por distrito - geom_jitter + coord_flip")
  pdf(file = "PDFs/peatones_por_dsitrito_2020.pdf")
p
dev.off()

p <- ggplot(PEATONES_2020) +
  geom_point(aes(x = FECHA, y = PEATONES, color = DISTRITO)) +
  facet_wrap(  ~ DISTRITO  ) +
  geom_smooth(aes(x = FECHA, y = PEATONES)) +
  labs( x = "Fecha", y = "Peatones contabilizados",
        title = "Peatones por fecha y separados por distrito - facet_wrap + geom_smooth")
pdf(file = "PDFs/peatones_por_fecha_separados_por_distrito_2020.pdf")
p
dev.off()

p <- tm_shape(mapa_2020) +
  tm_fill("PeatxDistr_log_2020", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito 2020",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))
  pdf(file = "PDFs/mapa_peatones_log_2020.pdf")
p
dev.off()

p <- tm_shape(mapa_prepandemia) +
  tm_fill("PeatxDistr_log_prepandemia", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito prepandemia",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))
  pdf(file = "PDFs/mapa_peatones_log_prepandemia_2020.pdf")
p
dev.off()

p <- tm_shape(mapa_confinamiento) +
  tm_fill("PeatxDistr_log_confinamiento", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito confinamiento",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))
  pdf(file = "PDFs/mapa_peatones_log_confinamiento_2020.pdf")
p
dev.off()

p <- tm_shape(mapa_estabilizacion) +
  tm_fill("PeatxDistr_log_estabilizacion", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito estabilizacion",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))
  pdf(file = "PDFs/mapa_peatones_log_estabilizacion_2020.pdf")
p
dev.off()

p <- tm_shape(mapa_segunda_ola) +
  tm_fill("PeatxDistr_log_segunda_ola", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito segunda_ola",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))
  pdf(file = "PDFs/mapa_peatones_log_segunda_ola_2020.pdf")
p
dev.off()

p <- ggplot(PEATONES_2021, aes(x=DISTRITO, y=PEATONES)) +
  geom_jitter(aes(color = DISTRITO )) +
  geom_boxplot(aes(x=DISTRITO, y=PEATONES) ) + 
  coord_flip() + 
  labs(x = "Distritos", y = "Peatones contabilizados", 
       title ="Peatones  por distrito - geom_jitter + coord_flip")
  pdf(file = "PDFs/peatones_por_dsitrito_2021.pdf")
p
dev.off()

p <- ggplot(PEATONES_2021) +
  geom_point(aes(x = FECHA, y = PEATONES, color = DISTRITO)) +
  facet_wrap(  ~ DISTRITO  ) +
  geom_smooth(aes(x = FECHA, y = PEATONES)) +
  labs( x = "Fecha", y = "Peatones contabilizados",
        title = "Peatones por fecha y separados por distrito - facet_wrap + geom_smooth")
pdf(file = "PDFs/peatones_por_fecha_separados_por_distrito_2021.pdf")
p
dev.off()

p <- tm_shape(mapa_2021) +
  tm_fill("PeatxDistr_log_2021", title="Peatones (escala logarítmica)") +
  tm_borders() +
  tm_layout(legend.outside = TRUE, 
            title="Peatones por distrito 2021",
            legend.format = list(text.separator = "-")) +
  tm_text("DISTRI_MAY", size=0.5) +  # nombres de los distritos 
  tm_compass(position=c("right", "top"))
  pdf(file = "PDFs/mapa_peatones_log_2021.pdf")
p
dev.off()

p <- ggplot(data=PEATONES_2021)+
  geom_point(mapping=aes(x=FECHA, y=PEATONES, color=months(FECHA)))
  pdf(file = "PDFs/peatones_por_mes_2021.pdf")
p
dev.off()
