##Created by Jhul Iparraguirre Castro##
#ESTE ARCHIVO REQUIERE OBTENER LA MATRIZ DATA1 DEL ARCHIVO PROYECTO_1.R ANTES DE SER EJECUTADO
pacman::p_load(dbscan, tidyverse, Rtsne, factoextra,dplyr,ggplot2,e1071,prodlim,FactoMineR,reshape2)
#Función para tratar de hacer funcionar readlines al correr todo el código a la vez (no funcionó)
user.input <- function(prompt) {
  if (interactive()) {
    cat(prompt)
    return(readline(prompt))
  } else {
    cat(prompt)
    return(readLines("stdin", n=1))
  }
}
#CORRER EL CODIGO HASTA CADA USER.INPUT, PORQUE R NO ESPERA AL INPUT DEL USUARIO Y TOMA LA SIGUIENTE LINEA COMO SI FUERA EL INPUT
#La creación de la Playlist--------
load("~/GitHub/Mineria-de-datos/proyectos/Proyecto1/Lista_con_clusters.Rda")
while (is.null(data1)){
  writeLines("seleccione como directorio de trabajo, el directorio en donde se guardo Lista_con_clusters.Rda :")
  setwd(readLines(file("stdin"), n = 1L))
  load("~/GitHub/Mineria-de-datos/proyectos/Proyecto1/Lista_con_clusters.Rda")
}
#ingresa el track_id
writeLines("**********************************************************
===Programa para crear una lista a partir de una canción===
***Creado por: Jhul Iparraguirre Castro***
**********************************************************
Ingrese 1 o 2, dependiendo de la opción que desea ingresar:
1.Id de la Canción
2.Nombre del artista y título de la canción")

choice<-user.input("Respuesta:")
#Comprobaciones de que los inputs esten correctos
while (choice!=1 & choice!=2){
  print("Ingrese una Opción valida entre 1 y 2:")
  choice<-user.input("Respuesta:")
}
if (choice==1){writeLines("Ingrese el Id de la canción")
  song_id=user.input("Respuesta:")
  #con el match encontrar el cluster al que pertenece
  song_cluster=data1[match(song_id, data1$track_id),"cl$cluster"]
  while (is.null(song_cluster)){writeLines("El Id de la canción no se encontró en la base de datos
                                            Ingrese otro Id de alguna canción:")
    song_id=user.input("Respuesta:")
    song_cluster=data1
  }
}
if(choice==2){writeLines("Ingrese el nombre del artista de la canción")
  song_artist=user.input("Respuesta:")
  artist_match=which(song_artist %in% data1$artist_name)
  while(is.null(artist_match)){writeLines("El nombre del artista no se encontró en la base de datos")
    writeLines("Ingrese el nombre del artista de la canción")
    song_artist=user.input("Respuesta:")
    artist_match=which(song_artist %in% data1$artist_name)
  }
  writeLines("Ingrese el nombre de la canción")
  song_name=user.input("Respuesta:")
  #con el match encontrar el cluster al que pertenece
  song_cluster=data1[match(song_name,data1[artist_match,"artist_name"]),"cl$cluster"]
  while(is.null(song_cluster)){writeLines("El nombre de la canción no se encontró en la base de datos del artista")
    writeLines("Ingrese el nombre de la canción")
    song_name=user.input("Respuesta:")
    song_cluster=data1[match(song_name,data1[artist_match,"artist_name"]),"cl$cluster"]
  }
}

#Una vez tenemos el cluster, crear una lista para obtener todas las canciones que pertenecen a ese cluster
list_song_cluster=which(data1$`cl$cluster` %in% song_cluster)
#Un dataframe vacío que va a ser la lista de reproducción de 3 horas
song_list <- data.frame(matrix(ncol = 37, nrow = 0))
colnames(song_list) <- colnames(data1)
#Para esta parte se requiere restablecer la semilla, sino lo que pasará es que la canción que se seleccione será siempre la misma
set.seed(NULL)
while (sum(song_list$duration_ms)<10800000){#Condición para que llegue a las 3 horas de duración
  #Selecciona la canción a partir de tomar una muestra de 1 canción aleatoria
  song_selected=data1[list_song_cluster[sample(1:length(list_song_cluster),size=1)],]%>% 
    as.data.frame()
  while (!is.na(row.match(song_selected,song_list))){#comprobación de que no se repitan canciones
    #Selecciona la canción a partir de tomar una muestra de 1 aleatoria
    song_selected=data1[list_song_cluster[sample(1:length(list_song_cluster),size=1)],]%>% 
      as.data.frame()}
  song_list <-rbind(song_list, song_selected)#Agrega la canción a la lista
}
song_list<-song_list%>%select(artist_name,track_name,duration_ms)
writeLines("Lista Terminada")
head(song_list)