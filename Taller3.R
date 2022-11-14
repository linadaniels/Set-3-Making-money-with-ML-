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
       leaflet, ## Visualizaciones din?micas
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata) ## Get OSM's data
#cargar bases
test <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/dataPS3/dataPS3/test.Rds")
train<- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/dataPS3/dataPS3/train.Rds")

##class
train
trainc<-train
class(train)
skim(train)
leaflet() %>% addTiles() %>% addCircleMarkers(data=train)
str_to_lower(string = train$description)
x <- "[:space:]+[:digit:]+[:space:]+mts"
train <- train %>% 
  mutate(new_surface = str_extract(string=description , pattern= x))
table(train$new_surface) %>% sort() %>% head()
x1 <- "[:space:]+[:digit:]+[:space:]+"
x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x3 <- "[:space:]+[:digit:]+"
train$new_surface <- NA
for (i in c("mts","m2","mt2","mts2","metros","cuadrado","mtro","mtr2")){
  train <- train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface))
}

for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2"," ","\n\n")){
  train$new_surface <- gsub(i,"",train$new_surface)
}

train$new_surface <- gsub(",",".",train$new_surface)
train$new_surface <- as.numeric(train$new_surface)
## replace surfare var
table(is.na(train$surface_total))
house$surface_total <- ifelse(is.na(train$surface_total),train$surface_covered,train$surface_total)
table(is.na(train$surface_total))
train$surface_total <- ifelse(is.na(train$surface_total),train$new_surface,train$surface_total)
table(is.na(train$surface_total))

#ahora para los baños

train$new_bathroom <- NA
for (i in c("baño","baños","bano","banos","tocadores")){
  train <- train %>% 
    mutate(new_bathroom = ifelse(is.na(new_bathroom)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_bathroom = ifelse(is.na(new_bathroom)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface),
           new_bathroom = ifelse(is.na(new_bathroom)==T,str_extract(string=description , pattern=paste0(x3,i)),new_surface)
    )
}

for (i in c("baño","baños","bano","banos","tocadores"," ","\n\n")){
  train$new_bathroom <- gsub(i,"",train$new_bathroom)
}
## replace bathroom var
table(is.na(train$bathroom))
house$bathroom <- ifelse(is.na(train$bathroom),train$new_bathroom,train$bathroom)
table(is.na(train$bathroom))

###       VECINOS ESPACIALES
## train MGN
# cargar manzanas
mnz <- st_read("input/mgn/MGN_URB_MANZANA.shp") %>% select(MANZ_CCNCT) %>% .[,]
mnz
leaflet() %>% addTiles() %>% 
  addPolygons(data=mnz , col="red") 
## unir dos conjuntos de datos basados en la geometría
join=st_nearest_feature
train <- st_as_sf( x=train,
                   coords=c("lon","lat"),
                   crs=4326)
class(train)
class(mnz)
train <- st_join(x=train, y=mnz)
train %>% select(rooms,bedrooms,bathrooms,surface_total,MANZ_CCNCT)

# intucion
new_train <- train[st_buffer(train[100,],200),]
new_mnz <- mnz[new_train,]

leaflet() %>% addTiles() %>%
  addPolygons(data=new_mnz,col="purple") %>%
  addCircles(data=new_train)
## unir dos conjuntos de datos basados en la distancia
new_train <- st_join(x=new_train , y=new_mnz , join=st_nn , maxdist=20 , k=1 , progress=F)
new_train %>% select(MANZ_CCNCT.x,MANZ_CCNCT.y)

leaflet() %>% addTiles() %>% 
  addPolygons(data=new_mnz , col="purple" , label=new_mnz$MANZ_CCNCT) %>% 
  addCircles(data=new_train , label=new_train$MANZ_CCNCT.y)
## construir covariables
train <- train %>% group_by() %>%
  mutate(surface_mnz=mean(surface_total,na.rm=T)) %>% ungroup()

train %>% select(MANZ_CCNCT,surface_mnz,surface_total)
table(is.na(train$surface_total))
train$surface_total <- ifelse(is.na(house$surface_total),train$surface_mnz,train$surface_total)
table(is.na(train$surface_total))

###CENSO
##datos
#censoant <- import("input/mnz_censo_2018.rds")
#censobog <- import("input/mnz_censo_2018.rds")
#censovalle <- import("input/mnz_censo_2018.rds")
#censo <- merge(censoant, merge(censobog, censovalle))
## construir covariables
#house <- left_join(house,censo,by=c("MANZ_CCNCT"="COD_DANE_ANM"))

#table(is.na(house$rooms))

#house$rooms <- ifelse(is.na(house$rooms),house$med_H_NRO_CUARTOS,house$rooms)
#table(is.na(house$rooms))
###       vecinos espaciales

## obtener objeto sp
new_train_sp <- new_train %>% st_buffer(20) %>% as_Spatial() # poligonos
## obtener vecinos
nb_train = poly2nb(pl=new_train_sp , queen=T) # opcion reina
## vecinos del inmueble 32
#nb_house[[32]]

## test MGN
## unir dos conjuntos de datos basados en la geometría
join=st_nearest_feature
test <- st_as_sf( x=test,
                   coords=c("lon","lat"),
                   crs=4326)
class(test)
class(mnz)
test <- st_join(x=train, y=mnz)
test %>% select(rooms,bedrooms,bathrooms,surface_total,MANZ_CCNCT)

# intucion
new_test <- test[st_buffer(test[100,],200),]
new_mnz_te <- mnz[new_test,]

leaflet() %>% addTiles() %>%
  addPolygons(data=new_mnz_te,col="purple") %>%
  addCircles(data=new_test)
## unir dos conjuntos de datos basados en la distancia
new_test <- st_join(x=new_test , y=new_mnz_te , join=st_nn , maxdist=20 , k=1 , progress=F)
new_test %>% select(MANZ_CCNCT.x,MANZ_CCNCT.y)

leaflet() %>% addTiles() %>% 
  addPolygons(data=new_mnz_te , col="purple" , label=new_mnz_te$MANZ_CCNCT) %>% 
  addCircles(data=new_test , label=new_test$MANZ_CCNCT.y)
## construir covariables
test <- test %>% group_by() %>%
  mutate(surface_mnz=mean(surface_total,na.rm=T)) %>% ungroup()

test %>% select(MANZ_CCNCT,surface_mnz,surface_total)
table(is.na(test$surface_total))
test$surface_total <- ifelse(is.na(house$surface_total),test$surface_mnz,test$surface_total)
table(is.na(test$surface_total))


## obtener objeto sp
new_test_sp <- new_test %>% st_buffer(20) %>% as_Spatial() # poligonos
## obtener vecinos
nb_test = poly2nb(pl=new_test_sp , queen=T) # opcion reina



#########################
#dataframe to sf
train <- st_as_sf(x = train, ## datos
                  coords=c("lon","lat"), ## coordenadas
                  crs=4326) ## CRS

test <- st_as_sf(x = test, ## datos
                 coords=c("lon","lat"), ## coordenadas
                 crs=4326) ## CRS


leaflet() %>% addTiles() %>% addCircles(data=train)

#dividir bogot? y Medell?n

train_bog <- train %>% 
  subset(city == "Bogot? D.C")

train_med <- train %>% 
  subset(city == "Medell?n")

#Distancias####

#cbd
#Bogot?
bog_cbd <- geocode_OSM("Bolsa de Valores de Colombia, Bogot?", as.sf = T)
leaflet() %>% addTiles() %>% addCircles(data = bog_cbd)

#Medell?n
med_cbd <- geocode_OSM("Milla de Oro, Medell?n", as.sf = T)
leaflet() %>% addTiles() %>% addCircles(data = med_cbd)
#Distancias
#Bogot?
train_bog$dist_cbd <- st_distance(x=train_bog , y=bog_cbd)
#Medell?n
train_med$dist_cbd <- st_distance(x=train_med , y=med_cbd)

#Explorar OSM
available_features()

available_tags("leisure")
available_tags("shop")
available_tags("amenity")


#Transporte p?blico
#Bogot?
# objeto osm
osm = opq(bbox = getbb("Bogot? Colombia")) %>%
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

#Medell?n
# objeto osm
osm = opq(bbox = getbb("Medell?n Colombia")) %>%
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
#Bogot?
# objeto osm
osm = opq(bbox = getbb("Bogot? Colombia")) %>%
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

#Medell?n
# objeto osm
osm = opq(bbox = getbb("Medell?n Colombia")) %>%
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
#Bogot?
# objeto osm
osm = opq(bbox = getbb("Bogot? Colombia")) %>%
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


#Medell?n
# objeto osm
osm = opq(bbox = getbb("Medell?n Colombia")) %>%
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
#Medell?n
parques_med <- opq(bbox = getbb("Medell?n Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addPolygons(data=parques_med)

# Distancia
matrix_dist_parque_med <- st_distance(x=train_med , y=parques_med)
min_dist_parque_med <- apply(matrix_dist_parque_med , 1 , min)

#Bogot?
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

#Transporte p?blico
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


###       Estadisticas descriptivas test
#str_test<-str(te_hog_d)
#sumtable(te_hog_d)
#       exportamos
#data(te_hog_d)
#sumtable(te_hog_d)
#vartable <- vtable(te_hog_d,out='return')

