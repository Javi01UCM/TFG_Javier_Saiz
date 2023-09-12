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
