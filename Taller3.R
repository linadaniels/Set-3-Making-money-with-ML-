###Taller 3

#clear all
rm(list = ls())

# paquetes
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
#cargar bases
test <- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/dataPS3/dataPS3/test.Rds")
train<- readRDS("C:/Users/linit/Documents/semestre 8/Big Data/dataPS3/dataPS3/train.Rds")
