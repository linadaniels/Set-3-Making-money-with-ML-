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
house<- train
class(house)
skim(house)
leaflet() %>% addTiles() %>% addCircleMarkers(data=house)
house <- house %>% 
  mutate(new_surface = str_extract(string=description , pattern= x))
table(house$new_surface) %>% sort() %>% head()
x1 <- "[:space:]+[:digit:]+[:space:]+"
x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
house$new_surface <- NA
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2")){
  house <- house %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(x2,i)),new_surface))
}

for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2"," ","\n\n")){
  house$new_surface <- gsub(i,"",house$new_surface)
}

house$new_surface <- gsub(",",".",house$new_surface)
house$new_surface <- as.numeric(house$new_surface)

