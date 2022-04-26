pacman::p_load(dbscan, tidyverse, Rtsne, factoextra)
#Aca lee el archivo y lo convierte en un objeto
#Selecionar las columnas que mejor representan la cancion y evitar los datos no númericos
beats <- readRDS("~/GitHub/Mineria-de-datos/beats.rds") %>%
  select("danceability","energy","key","loudness","mode","speechiness","acousticness","instrumentalness","liveness","valence","tempo")%>%
  unique()

set.seed(42)
beats_sample<-beats[sample(1:nrow(beats),size=nrow(beats)/10),]
tsne_beats_sample <- Rtsne(beats_sample)%>%
  .$Y %>% 
  as.data.frame()
fviz_nbclust(tsne_beats_sample, kmeans, method = "silhouette")

ggplot(tsne_beats_sample, aes(V1, V2)) + geom_point()
rm(beats_sample,tsne_beats_sample)
tsne_beats <- Rtsne(beats)%>%
  .$Y %>% 
  as.data.frame()
ggplot(tsne_beats, aes(V1, V2)) + geom_point()
#dt=iris
#dt_sample<-dt[sample(1:nrow(dt),size=nrow(dt)/4),]
