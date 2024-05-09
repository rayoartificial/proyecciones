library("readxl")
library(dplyr)
library(ggplot2)
library(broom)

set.seed(0)

setwd("D:/Documentos/Rayo Artificial/Análisis de datos/Proyecciones/proyecciones")
ventas <- read_excel("ventas-anuales.xlsx")
datos_proy <- read_excel("ventas-anuales.xlsx", sheet = "Proyecciones")

