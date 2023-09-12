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
#--------------------clustering---------------------

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