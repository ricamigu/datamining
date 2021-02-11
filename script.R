library(dplyr)
library(caret)
library(naivebayes)
library(na.tools)
library(ggplot2)
library(class)
library(tidyverse)
library('rnoaa')
library(lubridate)
library(MASS)
require(devtools)
require(randomForest)

# Task 1 - Removing all the '-' regions and unnecessary columns from the data set
## funcao que torna o nosso dataset de treino limpo
clean_data <- function(fires){
  
  fires <- fires[,-(11:15)]   # removemos colunas desnecessárias para o trabalho
  fires <- fires %>% na.rm()
  
  # usado para tornar as latitudes e longitudes consistentes para os cálculos efetuados
  fires <- mutate_if(fires, 
                     is.character, 
                     str_replace_all, pattern = "º", replacement = ":")
  
  fires <- mutate_if(fires, 
                     is.character, 
                     str_replace_all, pattern = "'", replacement = ":")
  
  fires <- mutate_if(fires, 
                     is.character, 
                     str_replace_all, pattern = "::", replacement = "''")
  
  fires <- mutate_if(fires, 
                     is.character, 
                     str_replace_all, pattern = ",", replacement = ".")
  
  return(fires)
}

## funcao que torna o nosso dataset de test limpo
clean_test <- function(test) {
  
  test <- mutate_if(test, 
                    is.character, 
                    str_replace_all, pattern = "º", replacement = ":")
  
  test <- mutate_if(test, 
                    is.character, 
                    str_replace_all, pattern = "'", replacement = ":")
  
  test <- mutate_if(test, 
                    is.character, 
                    str_replace_all, pattern = "::", replacement = "")
  
  test <- mutate_if(test, 
                    is.character, 
                    str_replace_all, pattern = ",", replacement = ".")
  
  return(test)
  
}

# converter latitudes para valores numéricos
convert_lat <- function(new) {
  for(i in 1:nrow(new)) {
    
    if(length(strsplit(new$lat[i], "[:]")[[1]]) == 1) next
    
    if(length(strsplit(new$lat[i], "[ ]")[[1]]) > 1) {
      lat_str <- strsplit(new$lat[i], "[ ]")
      lat_str <- strsplit(lat_str[[1]][2], "[:]")
      d <- as.numeric(lat_str[[1]][1])
      m <- as.numeric(lat_str[[1]][2])
      s <- as.numeric(lat_str[[1]][3])
      if(is.na(s)==TRUE) {
        s <- as.numeric(substr(lat_str[[1]][3],1,4))
      }
      dd = d + m/60 + s/3600
      new$lat[i] <- dd
      next
    }
    
    lat_str <- strsplit(new$lat[i], "[:]")
    d <- as.numeric(lat_str[[1]][1])
    m <- as.numeric(lat_str[[1]][2])
    s <- as.numeric(lat_str[[1]][3])
    if(is.na(s)==TRUE) {
      s <- as.numeric(substr(lat_str[[1]][3],1,4))
    }
    dd = d + m/60 + s/3600
    new$lat[i] <- dd
  }
  return(new)
}

# converter longitudes para valores numéricos
convert_lon <- function(new) {
  for(i in 1:nrow(new)) {
    if(length(strsplit(new$lon[i], "[:]")[[1]]) == 1) next
    
    if(length(strsplit(new$lon[i], "[ ]")[[1]]) > 1) {
      lon_str <- strsplit(new$lon[i], "[ ]")
      lon_str <- strsplit(lon_str[[1]][2], "[:]")
      d <- as.numeric(lon_str[[1]][1])
      m <- as.numeric(lon_str[[1]][2])
      s <- as.numeric(lon_str[[1]][3])
      if(is.na(s)==TRUE) {
        s <- as.numeric(substr(lon_str[[1]][3],1,4))
      }
      dd = d + m/60 + s/3600
      new$lon[i] <- dd
      next
    }
    
    lon_str <- strsplit(new$lon[i], "[:E]")
    d <- as.numeric(lon_str[[1]][1])
    m <- as.numeric(lon_str[[1]][2])
    s <- as.numeric(lon_str[[1]][3])
    if(is.na(s)==TRUE) {
      s <- as.numeric(substr(lon_str[[1]][3],1,4))
    }
    dd = d + m/60 + s/3600
    new$lon[i] <- dd
  }
  return(new)
}

# funcao que cria a coluna tmax que contem as temperaturas máximas da estação mais próxima da localização
# usamos as latitudes e longitudes para associar a estação mais próxima e daí extrair a temperatura máxima
# esta função foi aproveitada do ficheiro dado no enunciado getTemperatureNOAA.R
get_temperature <- function(tempdata){
  
  options(noaakey = "oevwoFjsmYxdezFhXOqVfSbNnMxiBBhp")
  load("station_data.Rdata")
  
  for(i in 1: length(tempdata$id)){
    df <- data.frame(
      id = c(tempdata$district[i]), 
      latitude = c(tempdata$lat[i]),
      longitude = c(tempdata$lon[i]),
      stringsAsFactors = FALSE
    )
    nearby_stations <-  meteo_nearby_stations(lat_lon_df = df,
                                              station_data = station_data, radius = 1000, 
                                              var = c("TMAX"),
                                              year_min = 2015, year_max = 2015)
    
    #Get TMAX data
    weather_data <- ghcnd_search(nearby_stations[[1]]$id[3], var = c("TMAX") , date_min = tempdata$alert_date[i] , date_max = tempdata$alert_date[i])
    
    temp <- do.call(rbind.data.frame, weather_data['tmax'])
    print(tempdata$id[i])
    tempdata$tmax[i] <- temp$tmax[1]
    
  }
  return(tempdata)
}


# tornar o dataset clean
df <- read.csv("fires2015_train.csv",na.strings='-',encoding = "UTF-8")
new <- clean_data(df)
new2 <- convert_lat(new)
new2 <- convert_lon(new2)
new3 <- get_temperature(new2) # obter as temperaturas maximas

# tornar os valores NA da tmax em valores medios
max1 <- max(new3$tmax, na.rm = TRUE)
min1 <- min(new3$tmax, na.rm = TRUE)
med <- min1:max1
new3$tmax[is.na(new3$tmax)] <- with(new3, ave(tmax, med, FUN = function(x) median(x, na.rm = TRUE)))[is.na(new3$tmax)]

new3$lat <- as.numeric(new3$lat)
new3$lon <- as.numeric(new3$lon)

fires_test2 <- read.csv("fires2015_test.csv",na.strings='-',encoding = "UTF-8")
fires_test <- clean_test(fires_test2)
fires_test <- convert_lat(fires_test)
fires_test <- convert_lon(fires_test)
fires_test3 <- get_temperature(fires_test)
max2 <- max(fires_test3$tmax, na.rm = TRUE)
min2 <- min(fires_test3$tmax, na.rm = TRUE)
med <- min2:max2
fires_test3$tmax[is.na(fires_test3$tmax)] <- with(fires_test3, ave(tmax, med, FUN = function(x) median(x, na.rm = TRUE)))[is.na(fires_test3$tmax)]
fires_test3$lat <- as.numeric(fires_test3$lat)
fires_test3$lon <- as.numeric(fires_test3$lon)


# --------------------------------------------------------
# Task 2 - Data exploratory analysis

print(ggplot(new, aes(x=origin)) + geom_bar() + theme(axis.text = element_text(size=15)))
print(ggplot(new, aes(x=total_area, y=region)) + geom_bar(stat = "identity"))

# --------------------------------------------------------
# Task 3 - Predictive Modelling


fire_temp <- new3[,c(2,3,6,7,8,9,11,12,13,16)] # escolher as coluna a usar no train
fire_temp$alert_date <- ymd(fire_temp$alert_date) # tornar no formato year-month-day
fire_temp <- fire_temp %>% mutate_if(is.character,as.factor)
modelo <- randomForest(cause_type ~.,data=fire_temp,ntree=1000,importance=TRUE) # modelo
test_temp <- fires_test3[,c(2,3,6,7,8,9,16,17,18)] 
test_temp$alert_date <- ymd(test_temp$alert_date) # tornar no formato year-month-day
test_temp <- test_temp %>% mutate_if(is.character,as.factor)
pred <- predict(modelo,test_temp,type="class") # previsao do modelo


# ----------------------submissao---------------------------------------
# codigo para extrair para o ficheiro submit.csv para submissão
submit <- data.frame(matrix(ncol=0, nrow=3218))
submit$id <- fires_test3$id
submit$cause_type <- pred
write.csv(submit , "submit.csv", row.names=FALSE)
