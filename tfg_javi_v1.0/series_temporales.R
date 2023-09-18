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

#Una vez tenemos cargados los datasets pasamos al mismo formato todos los datos de los 3 dataset

PEATONES_2019$FECHA<-as.Date(PEATONES_2019$FECHA, "%d-%m-%y")
PEATONES_2019$HORA<-as.character(PEATONES_2019$HORA)

PEATONES_2021$FECHA<-as.Date(PEATONES_2021$FECHA, "%d-%m-%y")
PEATONES_2021$HORA<-as.character(PEATONES_2021$HORA)

PEATONES_2020$FECHA<-as.Date(PEATONES_2020$FECHA, "%d-%m-%y")
PEATONES_2020$HORA<-format(as.POSIXct(PEATONES_2020$HORA), format = "%H:%M")
PEATONES_2020$HORA<-as.character(PEATONES_2020$HORA)

#Unimos los datos de PEATONES_2019, PEATONES_2020 Y PEATONES_2021
PEATONES_TOTAL <- rbind(PEATONES_2019,PEATONES_2020)
PEATONES_TOTAL <- rbind(PEATONES_TOTAL,PEATONES_2021)

#-------------------------------Series temporales-------------------------------------

#Hacer con calle Fuencarral porque por ejemplo en la gran via no ha dtos a finales de 2020
# y de Fuencarral si, el dataset contendrá las observaciones que tiene el dataset PEATONES_TOTAL
#cuyo distrito sea "chamberí", la hora sean las "12:00", el nombre vial sea "Calle Génova" y
#el campo observaciones_direccion sea "Acera Pares"
nuevo_dataset <- subset(PEATONES_TOTAL, DISTRITO == "Chamberí" & HORA == "12:00" & NOMBRE_VIAL =="Calle Génova"
                        & OBSERVACIONES_DIRECCION == "Acera Pares")

#lo primero es crear un objeto timeseries. Esto lo haremos con la
#función ts a partir de una tabla de datos.

peatones=ts(nuevo_dataset[,c(4)],start=c(2019,244),frequency = 365)
#La fecha por la que empieza el dataset con el que vamos a trabajar es el 1 de Septiembre
# de 2019, el 1 de Septiembre es el dia número 244 del año
print(peatones)
autoplot(peatones) + ggtitle("Peatones por año en Madrid") + xlab("Año") + ylab("Número de peatones")
#La serie o es estacionaria ni en media ni en varianza
#Es una serie temporal heterocedastica
#No es una serie estacionaria (la varianza no es cte a lo largo del tiempo)
#La serie no muestra una tendencia clara
#La serie no es estacional puesto que no podemos observar en ella un patron sistematico que se 
#repita periodicamente

#Descomposición de la serie temporal
#Guardamos los componentes de la descomposición estacional en peatones_Comp

peatones_Comp<- decompose(peatones, type=c("multiplicative"))

# peatones_Comp es una lista en la que se almacenan las distintas componentes
#Representamos los componentes de la serie obtenidos.

autoplot(peatones_Comp)

#En v_cordoba_E_Comp tenemos guardadas las tablas de los diferentes componentes que 
#se obtienen de la descomposición estacional
print(peatones_Comp)

peatones_Comp$seasonal

#Queremos hacer una predicción de 15 dias, el 100% de los datos serian hasta
#la fecha correspodiente con(2021,181), restandole los 15 dias que queremos predecir para luego
#hacer la comparación sería 181-15=166

peatones_TR<-window(peatones,start=c(2019,244), end=c(2021,166)) #desde el principio hasta una semana antes de los
#datos del dataset
autoplot(peatones_TR)

#Métodos de suavizado

#Modelo de alisado simple

peatones_alisado_simple=ses(peatones_TR, h=15)

summary(peatones_alisado_simple)

autoplot(peatones_alisado_simple) #Muestra la gráfica de las observaciones y predicciones

#Representamos los valores observados y los suavizados con la predicción para 15
#días
autoplot(peatones_alisado_simple) +
  autolayer(fitted(peatones_alisado_simple), series="Fitted") +
  ylab("peatones") + xlab("dia")

#---------------Alisado doble de Holt----------------------------------------------
peatones_holt <- holt(peatones_TR, h=15) #15 predicciones hacia delante
summary(peatones_holt) #Muestra el valor de alpha y Beta, predicciones e intervalos de confianza
autoplot(peatones_holt) #Gráfica de las observaciones y predicciones
autoplot(peatones_holt) +
  autolayer(fitted(peatones_holt), series="Fitted") +
  ylab("precio") + xlab("dia")

#Representar las funciones de autocorrelación simple y parcial

#Calculamos las autocorrelaciones simples hasta el retardo 48
ggAcf(peatones_TR, lag=48)

#Calculamos las autocorrelaciones parciales hasta el retardo 48
ggPacf(peatones_TR,lag=48) 

#Para ver únicamente los valores de las funciones ACF y PACF se usa

acf(peatones_TR, lag.max =20, plot =F)

pacf(peatones_TR, lag.max =20, plot =F)

#Ajuste con la función auto.arima
fit <- auto.arima(peatones_TR)
checkresiduals(fit)

print(fit)

#prediccion para 15 dias

arima_fit <- forecast(fit,h=15)

#representacion gráfica de la predicción

autoplot(arima_fit)

#----Comparaciones resultados series temporales------
#Obtengo los dias que había reservado que son los ultimos 15 dias y los comparo con los 15 dias
#predichos
real<-window(peatones,start=c(2021,167), end=c(2021,181))
valores_arima<-arima_fit$mean

cbind("Real" = real, "Prediccion" =valores_arima) %>%
  autoplot() + xlab("") + ylab("") +   ggtitle("Numero de peatones prediccion vs real")

#----------------------------------------------------------------------------------------
#Paso a pdf todas las graficas y imagenes realizadas durante el estudio

#---------------------Series temporales-------------------------------------

p <- autoplot(peatones) + ggtitle("peatones_por_año_en_madrid") + xlab("Año") + ylab("Número de peatones")
pdf(file = "PDFs/Peatones por año en Madrid.pdf")
p
dev.off()

p <- autoplot(peatones_Comp)
pdf(file = "PDFs/descomposicion_serie_multiplicativo.pdf")
p
dev.off()

p <- autoplot(peatones_alisado_simple)
pdf(file = "PDFs/alisado_simple_peatones.pdf")
p
dev.off()

p <- autoplot(peatones_holt)
pdf(file = "PDFs/alisado_doble_Holt.pdf")
p
dev.off()

p <- ggAcf(peatones_TR, lag=48)
pdf(file = "PDFs/función_autocorrelación_simple_ACF.pdf")
p
dev.off()

p <- ggPacf(peatones_TR,lag=48)
pdf(file = "PDFs/función_autocorrelación_parcial_PACF.pdf")
p
dev.off()

p <- autoplot(arima_fit)
pdf(file = "PDFs/arima.pdf")
p
dev.off()

p <- cbind("Real" = predicho, "Prediccion" =valores_arima) %>%
  autoplot() + xlab("") + ylab("") +   ggtitle("Numero de peatones prediccion vs real")
pdf(file = "PDFs/real_predicho_arima.pdf")
p
dev.off()
