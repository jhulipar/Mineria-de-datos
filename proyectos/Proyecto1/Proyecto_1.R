##Created by Jhul Iparraguirre Castro##
#Librerías ocupadas
pacman::p_load(tidyverse, kknn)

#TRABAJO CON LA MUESTRA DE LOS DATOS----
#Cambiar directorio si los datos iniciales no se encuentran en esa ubicación
#Acá lee el archivo y lo convierte en un objeto Data Frame
#Seleccionar las columnas que mejor representan la canción y evitar los datos no numéricos
beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds") %>%
  select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence)%>%
  unique()#Evitar los datos que se repiten
#Establecer una semilla para obtener resultado repetibles
set.seed(45)
#Tomar una muestra de la matriz
#(ESTA PARTE ES IMPORTANTE PARA DESPUÉS PODER HACE UN CORRECTO PROCEDIMIENTO CON TODOS LOS DATOS, PUES AL TENER UNA MUESTRA SE PUEDE TRABAJAR CON LOS DATOS
#SIN REQUERIR DE UN SERVIDOR CON MUCHA MEMORIA, PERO ESTA MUESTRA SOLO SIRVE PARA PODER ENCONTRAR UN NÚMERO IDEAL DE CLUSTERS PARA K-MEANS)
beats_sample<-beats[sample(1:nrow(beats),size=nrow(beats)/10),]%>%
  scale()%>% #Hacer que los datos tengan la misma escala numérica
  as.data.frame()
#Usar Tsne para reducir dimensiones y poder trabajar mejor con los datos
tsne_beats_sample <- Rtsne(beats_sample,num_threads=0)%>%
  .$Y %>% 
  as.data.frame()
#Graficar los datos (si queremos)
##ggplot(tsne_beats_sample, aes(V1, V2)) + geom_point()

#Mostrar (si queremos) como varia la silueta en función del nr de clusters, para poder escoger un número ideal de clusters
#CABE MENCIONAR QUE ESTA FUNCIÓN FUE USADA EN UN COMPUTADOR CON 16GB DE RAM. 
#POR LO TANTO, SI SE OCUPA EN UN COMPUTADOR CON MENOR RAM, SE REQUIERE REDUCIR AÚN MÁS LA MUESTRA EN LA LINEA 15
##fviz_nbclust(tsne_beats_sample, kmeans, method = "silhouette")

#Ejecutar el método de k-means para el k nr de centroides que muestra la linea anterior, en este caso 4 hace más sentido
cl<-kmeans(tsne_beats_sample,4)
beats_sample <- cbind(beats_sample, cl$cluster)

#Visualizar los datos y sus clusters (Si queremos)
##plot(tsne_beats_sample, col = cl$cluster)
##points(cl$centers, col = 1:4, pch = 8)

#Resaltar que cada cluster comprenderá canciones de varios y diferentes géneros musicales 
#debido a que el cluster no se basa en los atributos de los géneros sino en los atributos del audio de la canción,
#por lo que podemos encontrar que algunas canciones que comparten atributos similares entre géneros, a pesar de que fueron etiquetados en diferentes géneros.



#Boxplot para poder interpretar el significado de los clusters obtenido anteriormente----
beats.m <- melt(beats_sample, id.vars = "cl$cluster")
boxplot(value ~ `cl$cluster`, data = beats.m,
        boxwex = 0.04, at= 1:4 - 0.175,
        subset=variable=="danceability",
        main = "Componentes de las canciones según cluster",
        col = "lightsalmon",
        xlab = "Nr de Cluster",
        ylab = "Valores escalados",
        yaxs="i",outline=FALSE)
boxplot(value ~ `cl$cluster`, data = beats.m, add = TRUE,
        at= 1:4 - 0.125,
        boxwex = 0.04,outline=FALSE,
        subset = variable == "energy",col = 2)
boxplot(value ~ `cl$cluster`, data = beats.m, add = TRUE,
        at= 1:4 - 0.075,
        boxwex = 0.04,outline=FALSE,
        subset = variable == "loudness",col = 3)
boxplot(value ~ `cl$cluster`, data = beats.m, add = TRUE,
        at= 1:4 - 0.025,
        boxwex = 0.04,outline=FALSE,
        subset = variable == "speechiness",col = 4)
boxplot(value ~ `cl$cluster`, data = beats.m, add = TRUE,
        at= 1:4 + 0.025,
        boxwex = 0.04,outline=FALSE,
        subset = variable == "acousticness",col = 5)
boxplot(value ~ `cl$cluster`, data = beats.m, add = TRUE,
        at= 1:4 + 0.075,
        boxwex = 0.04,outline=FALSE,
        subset = variable == "instrumentalness",col = 6)
boxplot(value ~ `cl$cluster`, data = beats.m, add = TRUE,
        at= 1:4 + 0.125,
        boxwex = 0.04,outline=FALSE,
        subset = variable == "liveness",col = 7)
boxplot(value ~ `cl$cluster`, data = beats.m, add = TRUE,
        at= 1:4 + 0.175,
        boxwex = 0.04,outline=FALSE,
        subset = variable == "valence",col = 8)
legend("topleft",cex = 0.6, c("danceability", "liveness","energy","loudness",
                     "speechiness","acousticness","instrumentalness","valence"),
       fill = c("lightsalmon",2,3,4,5,6,7,8))
#En los clusters se puede apreciar que las componentes que principalmente afectan los grupos son danceability, liveness y energy,
#por lo cual estos grupos están separados según que tan "melancólicas" son las canciones,
#a su vez, se puede ver que en los cluster 1 y 2 hay valores bajos en acousticness y speechiness, por otro lado en los cluster 3 y 4 es al contrario,
#por lo cual, se puede ver una relación entre que tan "melancólica" es una canción según su acústica y letra.
#En conclusión, los grupos se ven que tienen sentido lógico, por lo cual se va a usar esta misma clusterización para todos los datos.


#Eliminar los datos anteriormente usados para poder ocupar el máximo de memoria posible al momento de trabajar con todos los datos-----

rm(beats_sample,tsne_beats_sample,beats.m,cl)

#TRABAJO CON LA BASE COMPLETA DE LOS DATOS (ejecutar solo está parte del código si se quiere obtener la lista necesaria para el otro archivo)----
#Cambiar directorio si los datos iniciales no se encuentran en esa ubicación
beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds")%>%
  select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence)%>%
  scale()
set.seed(45)
#Usar Tsne para reducir dimensiones y poder trabajar mejor con los datos
tsne_beats <- Rtsne(beats,num_threads=0,check_duplicates=F)%>%
  .$Y %>% 
  as.data.frame()
#Graficar los datos (si queremos)
##ggplot(tsne_beats, aes(V1, V2)) + geom_point()

#Ejecutar el método de k-means para los 4 grupos explicados anteriormente
cl<-kmeans(tsne_beats,4)
#Visualizar los datos y sus clusters(si queremos)
##plot(tsne_beats, col = cl$cluster)
##points(cl$centers, col = 1:4, pch = 8)
#Asignar los clusters a la matriz original de canciones
data1 <- cbind(readRDS("~/GitHub/Mineria-de-datos/beats.rds"), cl$cluster)
#Guardar la data completa con sus clusters para no tener que re calcular cada vez que se necesite una Playlist de alguna canción
save(data1, file = "Lista_con_clusters.Rda")
print("Lista guardada en:")
#Mostrar en donde se guardo la lista
print(getwd())
#Otro modelo de Clustering----
#En este otro modelo de Clustering, se utilizó DBScan, pero los resultados obtenidos no fueron utilizados
set.seed(45)
#Cambiar directorio si los datos iniciales no se encuentran en esa ubicación
#Acá lee el archivo y lo convierte en un objeto Data Frame
#Seleccionar las columnas que mejor representan la canción y evitar los datos no numéricos
beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds")%>%
  select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo,duration_ms,mode,key)%>%
  unique()%>%
  scale()

#Usar Tsne para reducir dimensiones y poder trabajar mejor con los datos
tsne_beats <- Rtsne(beats,num_threads=0,check_duplicates=F)%>%
  .$Y %>% 
  as.data.frame()

#Graficar los datos (si queremos)
##ggplot(tsne_beats, aes(V1, V2)) + geom_point()
#DBScan para asignar clusters a los datos
#La razón por la que se utilizaron estos hiperparametros es para verificar si se puede obtener los 4 clusters que se obtuvieron utilizando k-means
modelo_dbscan <- dbscan(tsne_beats, eps = 0.3, minPts = 15)
#Mostrar el nr de clusters obtenidos
modelo_dbscan$cluster %>% max()
#visualizar los datos y verificar si el modelo hizo el clustering como se esperaba
ggplot(tsne_beats, aes(V1, V2, col = factor(modelo_dbscan$cluster))) + 
  geom_point(alpha = 0.5) +
  theme(legend.position = "none")
#En el plot se puede visualizar que efectivamente dbscan no es un modelo muy efectivo para poder hacer
#clustering con los datos y obtener resultados tan buenos como los presentados con k-means,
#para estos datos k-means se acomoda bastante bien y arrojó resultados con sentido logico y aplicables.
#Por esta razón, no se utilizó DBScan como modelo final con estos datos.
#Cualquier otro modelo de clustering no se pudo habe aplicado directamente con los datos, 
#pues requieren mucho espacio de almacenamiento, por lo cual habría que reducir los datos de entrada y
#por lo tanto, los datos de salida, resultando una lista con muchas menos posibilidades.
