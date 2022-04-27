pacman::p_load(dbscan, tidyverse, Rtsne, factoextra)
#Aca lee el archivo y lo convierte en un objeto
#Selecionar las columnas que mejor representan la cancion y evitar los datos no númericos
beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds") %>%
  select(danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,time_signature,duration_ms)%>%
  unique()

set.seed(40)
beats_sample<-beats[sample(1:nrow(beats),size=nrow(beats)/10),]%>%
  scale()%>%
  as.data.frame()
tsne_beats_sample <- Rtsne(beats_sample,num_threads=0)%>%
  .$Y %>% 
  as.data.frame()
#Graficar los datos
ggplot(tsne_beats_sample, aes(V1, V2)) + geom_point()
#Mostrar como varia la silueta en funcion del nr de clusters
fviz_nbclust(tsne_beats_sample, kmeans, method = "silhouette")
#Ejecutar el metodo de k-means para el k nr de centroides
cl<-kmeans(tsne_beats_sample,4)
#data1 <- cbind(beats_sample, cl$cluster)
#Visualizar los datos y sus clusters
plot(tsne_beats_sample, col = cl$cluster)
points(cl$centers, col = 1:4, pch = 8)



rm(beats_sample,tsne_beats_sample)

################################################################################

beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds")%>%
  select(danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,time_signature,duration_ms)
tsne_beats <- Rtsne(beats,num_threads=0,check_duplicates=F)%>%
  .$Y %>% 
  as.data.frame()
#Graficar los datos
#ggplot(tsne_beats, aes(V1, V2)) + geom_point()
#No se puede usar el metodo de encontrar los nr de clusters porque son muchos datos y requiere un espacio exagerado de ram

#Ejecutar el metodo de k-means para el k nr de centroides
cl<-kmeans(tsne_beats,4)
#Visualizar los datos y sus clusters
plot(tsne_beats, col = cl$cluster)
points(cl$centers, col = 1:4, pch = 8)
#Asignar los clusters a la matriz original de canciones

data1 <- cbind(beats, cl$cluster)


#It is worth highlighting that each cluster will comprise songs from several and different music genres due to the cluster is not based on the 
#genres attributes but it is based on the song audio attributes, thus we may find some songs that share similar attributes -inter-genres- despite 
#they were tagged into different genres.