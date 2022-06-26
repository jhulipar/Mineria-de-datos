# cargo librerias
pacman::p_load(tidymodels, kknn,tidymodels, discrim,kernlab,MLmetrics)
set.seed(44)
# define funciones de tidymodels por defecto
tidymodels_prefer()

train_data<-read.csv("~/GitHub/Mineria-de-datos/proyectos/Proyecto 2/ALUMNOS-trainData.csv")%>% 
  unique() %>%
  sample_n(10000)
#Limpiar data
sapply(train_data, function(x) sum(is.na(x))) #Comprueba cantidad de NA
train_data =train_data[!is.na(train_data$departure_time),] #Borrar NA 
train_data =train_data[!duplicated(train_data$id),] #Borrar id duplicado
train_data$class<-with(train_data,ifelse(noshow>=4,1,0)) #Asignar a class 0 y 1 dependiendo de NoShow
train_data$noshow <- NULL #Borrar NoShow para entrenar correctamente
data_split_strat <- initial_split(train_data, prop = 3/4, strata = class)
train_data <- training(data_split_strat)
test_data  <- testing(data_split_strat)

train_data %>% 
  count(class) %>% 
  mutate(prop = n/sum(n))
train_data<-mutate(train_data,class=factor(class),date = lubridate::as_date(date))%>%
  mutate_if(is.character, as.factor)
#Seleccionar las variables
train_data<-select(train_data,class,date,departure_time,destination)
# generamos la receta
receta <- 
  recipe(class ~ ., data = train_data) 

receta

#definimos el modelo con 1 grado polinomial
modelo <- svm_poly(degree = 1) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()


modelo

# definimos funcion fitea igual que otras oportunidades
fitea <- function(mod){
  
  modelo_fit <- 
    workflow() %>% 
    add_model(mod) %>% 
    add_recipe(receta) %>% 
    fit(data = train_data)
  
  model_pred <- 
    predict(modelo_fit, test_data, type = "prob") %>% 
    bind_cols(test_data) 
  
  return(model_pred %>% 
           roc_auc(truth = Exited, .pred_0))
}

# ajustamos el modelo
fitea(modelo)

# generamos otra funcion para fitear en diferentes grados polinomiales
fitea_polySVM <- function(grado){
  
  mod <- svm_poly(degree = grado) %>% 
    set_engine("kernlab") %>% 
    set_mode("classification") %>% 
    translate()
  
  modelo_fit <- 
    workflow() %>% 
    add_model(mod) %>% 
    add_recipe(receta) %>% 
    fit(data = train_data)
  
  model_pred <- 
    predict(modelo_fit, test_data, type = "prob") %>% 
    bind_cols(test_data) 
  
  return(model_pred %>% 
           metrics(class, .pred_class))
}
mod <- svm_poly(degree = 1) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()

modelo_fit <- 
  workflow() %>% 
  add_model(mod) %>% 
  add_recipe(receta) %>% 
  fit(data = train_data)

model_pred <- 
  predict(modelo_fit, test_data, type = "prob") %>% 
  bind_cols(test_data)
F1_Score(test_data$is_female,model_pred$is_female)
fitea_polySVM(1)
fitea_polySVM(2)
fitea_polySVM(3)