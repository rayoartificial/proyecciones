library("readxl")
library(dplyr)
library(ggplot2)
library(broom)

set.seed(0)

setwd("D:/Documentos/Rayo Artificial/Análisis de datos/Proyecciones/proyecciones")
ventas <- read_excel("ventas-anuales.xlsx")
datos_proy <- read_excel("ventas-anuales.xlsx", sheet = "2024-2029")

# Análisis del PBI, la población y la ventas
model <- ventas %>% filter(Año %in% 1981:2022) %>% lm(Ventas ~ PBI + Población, data = .)
coefs <- tidy(model, conf.int = TRUE)
coefs

# Análisis 
model <- ventas %>% filter(Año %in% 1981:2022) %>% lm(log(Ventas) ~ log(PBI) + log(Población) + log(Tarifa), data = .)
model
summary(model)

coefs <- tidy(model, conf.int = TRUE)
coefs

ventas %>% filter(Año %in% 2023) %>% mutate( Ventas_proy = exp(predict(model, newdata=.)) )
