library("readxl")
library(dplyr)
library(ggplot2)
library(broom)

set.seed(0)

setwd("D:/Documentos/Rayo Artificial/Análisis de datos/Proyecciones/proyecciones")
ventas <- read_excel("ventas-anuales.xlsx")
datos_proy <- read_excel("ventas-anuales.xlsx", sheet = "Proyecciones")

# Análisis lineal
modelo_lineal <- ventas %>% 
  filter(Año %in% 1981:2022) %>% 
  lm(Ventas ~ PBI + Población, data = .)

ventas %>%
  filter(Año %in% 2023) %>%
  mutate( Ventas_proy = predict(modelo_lineal, newdata=.))

# Análisis logarítmico
modelo_log <- ventas %>% 
  filter(Año %in% 1981:2022) %>% 
  lm(log(Ventas) ~ log(PBI) + log(Población) + log(Tarifa), data = .)

ventas %>% 
  filter(Año %in% 2023) %>% 
  mutate( Ventas_proy = exp(predict(modelo_log, newdata=.)) )

# Proyecciones con el mejor modelo
modelo <- ventas %>% lm(log(Ventas) ~ log(PBI) + log(Población) + log(Tarifa), data = .)
modelo
summary(modelo)

coefs <- tidy(modelo, conf.int = TRUE)
coefs

# 95% de confianza
ventas_proy <- exp(predict(modelo, newdata = datos_proy[,2:4], interval = "confidence", level = 0.95) ) %>% as.data.frame()

proyecciones <- data.frame(Año=seq(1981,2029),
                           Ventas=c(ventas$Ventas,ventas_proy$fit) ) %>% filter(Año >= 2015)

intervalos <- ventas_proy %>% rbind(tail(ventas$Ventas, n=1), .) %>% cbind(Año=seq(2023,2029))

# 90% de confianza
ventas_proy90 <- exp(predict(modelo, newdata = datos_proy[,2:4], interval = "confidence", level = 0.90) ) %>% as.data.frame()
intervalos90 <- ventas_proy90 %>% rbind(tail(ventas$Ventas, n=1), .) %>% cbind(Año=seq(2023,2029))

