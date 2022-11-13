###Taller 3

#clear all
rm(list = ls())

# paquetes
install.packages("pacman")
install.packages("here")
install.packages("caret")
install.packages("leaps")
install.packages("vtable")
install.packages("st")
install.packages("raster")
install.packages("dplyr")

library(vtable)
require("leaps")
require("caret")
require("dplyr")
require("here")
require("tidyverse")
require(pacman) 
p_load(tidyverse,rio,skimr,viridis,
       gstat, ## variogram
       sf, ## leer/escribir/manipular datos espaciales
       leaflet, ## Visualizaciones dinámicas
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata) ## Get OSM's data
#cargar bases
test <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/dataPS3/dataPS3/test.Rds")
train<- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/dataPS3/dataPS3/train.Rds")

##class
train
trainc<-train
#train1<-train[sample(nrow(train),size=)]
class(train)
skim(train)
leaflet() %>% addTiles() %>% addCircleMarkers(data=train)
train <- train %>% 
  mutate(new_surface = str_extract(string=description , pattern= x))
table(train$new_surface) %>% sort() %>% head()
x1 <- "[:space:]+[:digit:]+[:space:]+"
x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
train$new_surface <- NA
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2")){
  train <- train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface))
}

for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2"," ","\n\n")){
  train$new_surface <- gsub(i,"",house$new_surface)
}

train$new_surface <- gsub(",",".",train$new_surface)
train$new_surface <- as.numeric(train$new_surface)

#dataframe to sf
train <- st_as_sf(x = train, ## datos
                  coords=c("lon","lat"), ## coordenadas
                  crs=4326) ## CRS

test <- st_as_sf(x = test, ## datos
                 coords=c("lon","lat"), ## coordenadas
                 crs=4326) ## CRS


leaflet() %>% addTiles() %>% addCircles(data=train)

#dividir bogotá y Medellín

train_bog <- train %>% 
  subset(city == "Bogotá D.C")

train_med <- train %>% 
  subset(city == "Medellín")

#Distancias####

#cbd
#Bogotá
bog_cbd <- geocode_OSM("Bolsa de Valores de Colombia, Bogotá", as.sf = T)
leaflet() %>% addTiles() %>% addCircles(data = bog_cbd)

#Medellín
med_cbd <- geocode_OSM("Milla de Oro, Medellín", as.sf = T)
leaflet() %>% addTiles() %>% addCircles(data = med_cbd)
#Distancias
#Bogotá
train_bog$dist_cbd <- st_distance(x=train_bog , y=bog_cbd)
#Medellín
train_med$dist_cbd <- st_distance(x=train_med , y=med_cbd)

#Explorar OSM
available_features()

available_tags("leisure")
available_tags("shop")
available_tags("amenity")


#Transporte público
#Bogotá
# objeto osm
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
# extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
# Obtener un objeto sf
bus_station_bog = osm_sf$osm_points %>% select(osm_id,amenity) 
bus_station_bog
# Pintar las estaciones de autobus
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_bog , col="red")
# Distancia
matrix_dist_bus_bog <- st_distance(x=train_bog , y=bus_station_bog)
min_dist_bus_bog <- apply(matrix_dist_bus_bog , 1 , min)

#Medellín
# objeto osm
osm = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
# extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
# Obtener un objeto sf
bus_station_med = osm_sf$osm_points %>% select(osm_id,amenity) 
bus_station_med
# Pintar las estaciones de autobus
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_med , col="red")
#Distancia
matrix_dist_bus_med <- st_distance(x=train_med , y=bus_station_med)
min_dist_bus_med <- apply(matrix_dist_bus_med , 1 , min)

#Centros comerciales
#Bogotá
# objeto osm
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="shop" , value="mall") 
class(osm)

# extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

# Obtener un objeto sf
mall_bog = osm_sf$osm_points %>% select(osm_id, shop) 
mall_bog
# Pintar los centros comerciales
leaflet() %>% addTiles() %>% addCircleMarkers(data=mall_bog , col="red")
#Distancia
matrix_mall_bog <- st_distance(x=train_bog , y=mall_bog)
min_mall_bog <- apply(matrix_mall_bog , 1 , min)

#Medellín
# objeto osm
osm = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="shop" , value="mall") 
class(osm)

# extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

# Obtener un objeto sf
mall_med = osm_sf$osm_points %>% select(osm_id, shop) 
mall_med

# Pintar los centros comerciales
leaflet() %>% addTiles() %>% addCircleMarkers(data=mall_med , col="red")

#Distancia
matrix_mall_med <- st_distance(x=train_med , y=mall_med)
min_mall_med <- apply(matrix_mall_med , 1 , min)


#Supermercados
#Bogotá
# objeto osm
osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="shop" , value="supermarket") 
class(osm)

# extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

# Obtener un objeto sf
supermarket_bog = osm_sf$osm_points %>% select(osm_id, shop) 
supermarket_bog

# Pintar los supermercados
leaflet() %>% addTiles() %>% addCircleMarkers(data=supermarket_bog , col="red")

#Distancia
matrix_supermarket_bog <- st_distance(x=train_bog , y=supermarket_bog)
min_supermarket_bog <- apply(matrix_supermarket_bog , 1 , min)


#Medellín
# objeto osm
osm = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="shop" , value="supermarket") 
class(osm)

# extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

# Obtener un objeto sf
supermarket_med = osm_sf$osm_points %>% select(osm_id, shop) 
supermarket_med

# Pintar los supermercados
leaflet() %>% addTiles() %>% addCircleMarkers(data=supermarket_med , col="red")

#Distancia
matrix_supermarket_med <- st_distance(x=train_med , y=supermarket_med)
min_supermarket_med <- apply(matrix_supermarket_med , 1 , min)


#Parques
## parques
#Medellín
parques_med <- opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addPolygons(data=parques_med)

# Distancia
matrix_dist_parque_med <- st_distance(x=train_med , y=parques_med)
min_dist_parque_med <- apply(matrix_dist_parque_med , 1 , min)

#Bogotá
parques_bog <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addPolygons(data=parques_bog)

# Distancia
matrix_dist_parque_bog <- st_distance(x=train_bog , y=parques_bog)
min_dist_parque_bog <- apply(matrix_dist_parque_bog , 1 , min)

#Unimos todo
train_bog$dist_bus = min_dist_bus_bog
train_med$dist_bus = min_dist_bus_med
train_bog$dist_parque = min_dist_parque_bog 
train_med$dist_parque = min_dist_parque_med
train_bog$dist_mall = min_mall_bog
train_med$dist_mall = min_mall_med
train_bog$dist_supermarket = min_supermarket_bog 
train_med$dist_supermarket = min_supermarket_med

train_final <- rbind(train_bog, train_med) 

#Test####
leaflet() %>% addTiles() %>% addCircles(data=test)


#cbd
#Cali
cal_cbd <- geocode_OSM("CAM, Cali", as.sf = T)
leaflet() %>% addTiles() %>% addCircles(data = cal_cbd)

test$dist_cbd <- st_distance(x=test , y=cal_cbd)

#Transporte público
#Cali
# objeto osm
osm = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osm)
# extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf
# Obtener un objeto sf
bus_station_cal = osm_sf$osm_points %>% select(osm_id,amenity) 
bus_station_cal
# Pintar las estaciones de autobus
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_cal , col="red")
# Distancia
matrix_dist_bus_cal <- st_distance(x=test , y=bus_station_cal)
min_dist_bus_cal <- apply(matrix_dist_bus_cal , 1 , min)


#Centros comerciales
#Cali
# objeto osm
osm = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="shop" , value="mall") 
class(osm)

# extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

# Obtener un objeto sf
mall_cal = osm_sf$osm_points %>% select(osm_id, shop) 
mall_cal
# Pintar los centros comerciales
leaflet() %>% addTiles() %>% addCircleMarkers(data=mall_cal , col="red")
#Distancia
matrix_mall_cal <- st_distance(x=test , y=mall_cal)
min_mall_cal <- apply(matrix_mall_cal , 1 , min)


#Supermercados
#Cali
# objeto osm
osm = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="shop" , value="supermarket") 
class(osm)

# extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

# Obtener un objeto sf
supermarket_cal = osm_sf$osm_points %>% select(osm_id, shop) 
supermarket_cal

# Pintar los supermercados
leaflet() %>% addTiles() %>% addCircleMarkers(data=supermarket_cal , col="red")

#Distancia
matrix_supermarket_cal <- st_distance(x=test , y=supermarket_cal)
min_supermarket_cal <- apply(matrix_supermarket_cal , 1 , min)

#Parques
#Cali
parques_cal <- opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addPolygons(data=parques_cal)

# Distancia
matrix_dist_parque_cal <- st_distance(x=test , y=parques_cal)
min_dist_parque_cal <- apply(matrix_dist_parque_cal , 1 , min)

#Unimos todo
test$dist_bus = min_dist_bus_cal
test$dist_parque = min_dist_parque_cal
test$dist_mall = min_mall_cal
test$dist_supermarket = min_supermarket_cal
