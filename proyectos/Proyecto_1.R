##Librerías ocupadas
pacman::p_load(dbscan, tidyverse, Rtsne, factoextra,dplyr,ggplot2,e1071,prodlim,FactoMineR,reshape2)

#TRABAJO CON LA MUESTRA DE LOS DATOS----
#Acá lee el archivo y lo convierte en un objeto Data Frame
#Seleccionar las columnas que mejor representan la canción y evitar los datos no numéricos
beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds") %>%
  select(danceability,energy,loudness,speechiness,acousticness,instrumentalness,liveness,valence,tempo)%>%
  unique()#Evitar los datos que se repiten
#Establecer una semilla para obtener resultado repetibles
set.seed(40)
#Tomar una muestra de la matriz
#(ESTA PARTE ES IMPORTANTE PARA DESPUÉS PODER HACE UN CORRECTO PROCEDIMIENTO CON TODOS LOS DATOS, PUES AL TENER UNA MUESTRA SE PUEDE TRABAJAR CON LOS DATOS
#SIN REQUERIR DE UN SERVIDOR CON MUCHA MEMORIA, PERO ESTA MUESTRA SOLO SIRVE PARA PODER ENCONTRAR UN NÚMERO IDEAL DE CLUSTERS PARA K-MEANS)
beats_sample<-beats[sample(1:nrow(beats),size=nrow(beats)/10),]%>%
  scale()%>% #Hacer que los datos tengan la misma escala numérica
  as.data.frame()
tsne_beats_sample <- Rtsne(beats_sample,num_threads=0)%>%
  .$Y %>% 
  as.data.frame()
#Graficar los datos (si queremos)
##ggplot(tsne_beats_sample, aes(V1, V2)) + geom_point()

#Mostrar como varia la silueta en función del nr de clusters, para poder escoger un número ideal de clusters
#CABE MENCIONAR QUE ESTA FUNCIÓN FUE USADA EN UN COMPUTADOR CON 16GB DE RAM. 
#POR LO TANTO, SI SE OCUPA EN UN COMPUTADOR CON MENOR RAM, SE REQUIERE REDUCIR AÚN MÁS LA MUESTRA EN LA LINEA 15
##fviz_nbclust(beats_sample, kmeans, method = "silhouette")

#Ejecutar el método de k-means para el k nr de centroides que muestra la figura anterior, en este caso 4
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
boxplot(value ~ `cl$cluster`, data = beats.m, add = TRUE,
        at= 1:4 + 0.225,
        boxwex = 0.04,outline=FALSE,
        subset = variable == "tempo",col = "seashell")
legend("topleft",cex = 0.6, c("danceability", "liveness","energy","loudness",
                     "speechiness","acousticness","instrumentalness","valence","tempo"),
       fill = c("lightsalmon",2,3,4,5,6,7,8,"seashell"))
#En el cluster 1 se puede apreciar que hay música con bajos valores en liveliness,danceability y tempo, lo que se puede traducir como canciones melancólicas y con alta acústica



#Eliminar los datos anteriormente usados para poder ocupar el máximo de memoria posible al momento de trabajar con todos los datos-----

rm(beats_sample,tsne_beats_sample,beats.m,cl)

#TRABAJO CON LA BASE COMPLETA DE LOS DATOS----(TERMINAR comentarios)----

beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds")%>%
  select(danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,time_signature,duration_ms)%>%
  scale()

tsne_beats <- Rtsne(beats,num_threads=0,check_duplicates=F)%>%
  .$Y %>% 
  as.data.frame()
#Graficar los datos (si queremos)
##ggplot(tsne_beats, aes(V1, V2)) + geom_point()

#Ejecutar el método de k-means para el k nr de centroides
cl<-kmeans(tsne_beats,4)
#Visualizar los datos y sus clusters
plot(tsne_beats, col = cl$cluster)
points(cl$centers, col = 1:4, pch = 8)
#Asignar los clusters a la matriz original de canciones
beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds")
data1 <- cbind(beats, cl$cluster)

#La creación de la Playlist-----(MOVER A OTRO ARCHIVO)--------
#ingresa el track_id
song_id=readline("ingresar track id:")
#con el match encontrar el cluster al que pertenece
song_cluster=data1[match(song_id, data1$track_id),"cl$cluster"]
#Una vez con el cluster, crear una lista para obtener todas las canciones que pertenecen a ese cluster
list_song_cluster=which(data1$`cl$cluster` %in% song_cluster)
#Un dataframe vacío que va a ser la lista de reproducción de 3 horas
song_list <- data.frame(matrix(ncol = 37, nrow = 0))
colnames(song_list) <- colnames(data1)
#Para esta parte se requiere restablecer la semilla, sino lo que pasará es que la canción que seleccione será siempre la misma
set.seed(NULL)
while (sum(song_list$duration_ms)<10800000){#Condición para que llegue a las 3 horas de duración
  #Selecciona la canción a partir de tomar una muestra de 1 aleatoria
  song_selected=data1[list_song_cluster[sample(1:length(list_song_cluster),size=1)],]%>% 
  as.data.frame()
  while (!is.na(row.match(song_selected,song_list))){#comprobación de que no se repitan canciones
    #Selecciona la canción a partir de tomar una muestra de 1 aleatoria
    song_selected=data1[list_song_cluster[sample(1:length(list_song_cluster),size=1)],]%>% 
    as.data.frame()}
  song_list <-rbind(song_list, song_selected)#Agrega la canción a la lista
}

#Otro modelo de Clustering----(TERMINAR, NO SE ME OCURRE QUE MODELO OCUPAR AIUDAAA)----
set.seed(40)
Sample5K<-sample(nrow(tsne_beats),nrow(tsne_beats)/20)
## Cluster the sample
DM5K = dist(tsne_beats[Sample5K,])
HC5K = hclust(DM5K, method="single")
Groups = cutree(HC5K, 8)
Groups[Groups>4] = 4
plot(tsne_beats[Sample5K,], pch=20, col=rainbow(4, alpha=c(0.2,0.2,0.2,1))[Groups])
##
Core = which(Groups<4)
library(class)
knnClust = knn(tsne_beats[Sample5K[Core], ], tsne_beats, Groups[Core])
plot(tsne_beats, pch=20, col=rainbow(3, alpha=0.1)[knnClust])

#cah.test <- HCPC(tsne_beats, graph=FALSE, nb.clust=-1)

# CAH with kmeans : work quickly
cl <- kmeans(tsne_beats, 4, iter.max=20)
cah <- HCPC(cl$centers, graph=FALSE, nb.clust=-1)
plot.HCPC(cah, choice="tree")





