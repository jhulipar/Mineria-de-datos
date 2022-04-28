#Librerías ocupadas
pacman::p_load(dbscan, tidyverse, Rtsne, factoextra,dplyr,ggplot2,e1071,prodlim)
#TRABAJO CON LA MUESTRA DE LOS DATOS----
#Acá lee el archivo y lo convierte en un objeto Data Frame
#Seleccionar las columnas que mejor representan la canción y evitar los datos no numéricos
beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds") %>%
  select(danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,time_signature,duration_ms)%>%
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
ggplot(tsne_beats_sample, aes(V1, V2)) + geom_point()
#Mostrar como varia la silueta en función del nr de clusters, para poder escoger un número ideal de clusters
#CABE MENCIONAR QUE ESTA FUNCIÓN FUE USADA EN UN COMPUTADOR CON 16GB DE RAM. 
#POR LO TANTO, SI SE OCUPA EN UN COMPUTADOR CON MENOR RAM, SE REQUIERE REDUCIR AÚN MÁS LA MUESTRA
fviz_nbclust(tsne_beats_sample, kmeans, method = "silhouette")
#Ejecutar el método de k-means para el k nr de centroides que muestra la figura anterior, en este caso 4
cl<-kmeans(tsne_beats_sample,4)
#data1 <- cbind(beats_sample, cl$cluster)
#Visualizar los datos y sus clusters
plot(tsne_beats_sample, col = cl$cluster)
points(cl$centers, col = 1:4, pch = 8)
#Boxplot para poder interpretar el número K obtenido anteriormente----(TERMINAR)----
bclust(beats_sample,centers=4,base.method = "kmeans")%>%
  boxplot()
#Eliminar los datos anteriormente usados para poder ocupar el máximo de memoria posible----------------------

rm(beats_sample,tsne_beats_sample)

#TRABAJO CON LA BASE COMPLETA DE LOS DATOS----(TERMINAR comentarios)----

beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds")%>%
  select(danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,time_signature,duration_ms)%>%
  scale()

tsne_beats <- Rtsne(beats,num_threads=0,check_duplicates=F)%>%
  .$Y %>% 
  as.data.frame()
#Graficar los datos (si queremos)
ggplot(tsne_beats, aes(V1, V2)) + geom_point()

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







#Anotaciones----
#Funcion replace en caso de que se necesite dejar sin NULL las casillas
#replace(.=="NULL", 0) # replace with NA
#####################---
##RAZONAMIENTO HECHO PARA ESCOJER EL NR DE CLUSTERS EN K-MEANS
#It is worth highlighting that each cluster will comprise songs from several and different music genres due to the cluster is not based on the 
#genres attributes but it is based on the song audio attributes, thus we may find some songs that share similar attributes -inter-genres- despite 
#they were tagged into different genres.