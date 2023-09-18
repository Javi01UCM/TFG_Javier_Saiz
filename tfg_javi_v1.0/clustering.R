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
#--------------------------Clustering------------------------------

#Hago una agrupación de PEATONES_TOTAL y una suma de los peatones para trabajar con el total
#de peatones por código postal
data_agrupacion <- PEATONES_TOTAL %>%
  group_by(CÓDIGO_POSTAL) %>%
  summarize(PEATONES=sum(PEATONES, na.rm=T))

#Añado el número de distirto para tener mas informacion

data_agrupacion$num_distrito <- c(1, 7, 1, 9, 10, 2, 1, 1, 7)

#Para que los nombres de las filas de amm en vez de estar numerados , sea el código postal
row.names(data_agrupacion)<-data_agrupacion$CÓDIGO_POSTAL

#algoritmo jerarquico simple
datos_clust_simple <- hclust(dist(data_agrupacion,method="euclidian"),method="single")
plot(datos_clust_simple)

#Visualizo el  dendograma
#Selecciono 3 clusteres, k = 3
fviz_dend(datos_clust_simple, k = 3,
          cex = 0.5, 
          k_colors =
            c("#2E9FDF", "#00AFBB",
              "#E7B800"),
          # Diferentes colores a los clusters
          color_labels_by_k = TRUE, 
          #añade un rectángulo alrededor
          rect = TRUE) +
  labs(title = "Dendograma (simple)")

#algoritmo jerarquico completo
datos_clust_completo <- hclust(dist(data_agrupacion,method="euclidian"),method="complete")
plot(datos_clust_completo)

#Selecciono 3 clusteres, k = 3
fviz_dend(datos_clust_completo, k = 3,
          cex = 0.5, 
          k_colors =
            c("#2E9FDF", "#00AFBB",
              "#E7B800"),
          # Diferentes colores a los clusters
          color_labels_by_k = TRUE, 
          #añade un rectángulo alrededor
          rect = TRUE) +
  labs(title = "Dendograma (completo)")

##algoritmo jerarquico medio
datos_clust_medio <- hclust(dist(data_agrupacion,method="euclidian"),method="average")
plot(datos_clust_medio)

#Selecciono 3 clusteres, k = 3
fviz_dend(datos_clust_medio, k = 3,
          cex = 0.5, 
          k_colors =
            c("#2E9FDF", "#00AFBB",
              "#E7B800"),
          # Diferentes colores a los clusters
          color_labels_by_k = TRUE, 
          #añade un rectángulo alrededor
          rect = TRUE) +
  labs(title = "Dendograma (medio)")


#kmeans

#Asignamos una semilla para garantizar la aleatoriedad de la primera asignación
set.seed(100)

#Creamos los clusteres mediante kmeans
data_agrupacion.kmeans<-kmeans(data_agrupacion,centers=4)
#Observamos los resultados y la calidad de los mismos
data_agrupacion.kmeans

#Represento los resultados
cod_post<-data_agrupacion$CÓDIGO_POSTAL
grupo<-data_agrupacion.kmeans$cluster
datosc<-data.frame(cod_post,grupo)



ggplot(datosc)+
  geom_point(mapping=aes(x=grupo, y=cod_post),color=grupo,size=5)

unique(PEATONES_TOTAL$DISTRITO)
unique(PEATONES_TOTAL$CÓDIGO_POSTAL)

p <- fviz_dend(datos_clust_simple, k = 3,
               cex = 0.5, 
               k_colors =
                 c("#2E9FDF", "#00AFBB",
                   "#E7B800"),
               # Diferentes colores a los clusters
               color_labels_by_k = TRUE, 
               #añade un rectángulo alrededor
               rect = TRUE) +
  labs(title = "Dendograma (simple)")
pdf(file = "PDFs/dendograma_simple.pdf")
p
dev.off()

p <- fviz_dend(datos_clust_completo, k = 3,
               cex = 0.5, 
               k_colors =
                 c("#2E9FDF", "#00AFBB",
                   "#E7B800"),
               # Diferentes colores a los clusters
               color_labels_by_k = TRUE, 
               #añade un rectángulo alrededor
               rect = TRUE) +
  labs(title = "Dendograma (completo)")
pdf(file = "PDFs/dendograma_completo.pdf")
p
dev.off()

p <- fviz_dend(datos_clust_medio, k = 3,
               cex = 0.5, 
               k_colors =
                 c("#2E9FDF", "#00AFBB",
                   "#E7B800"),
               # Diferentes colores a los clusters
               color_labels_by_k = TRUE, 
               #añade un rectángulo alrededor
               rect = TRUE) +
  labs(title = "Dendograma (medio)")
pdf(file = "PDFs/dendograma_medio.pdf")
p
dev.off()

p <- ggplot(datosc)+
  geom_point(mapping=aes(x=grupo, y=cod_post),color=grupo,size=5)
pdf(file = "PDFs/cluster_kmeans.pdf")
p
dev.off()
