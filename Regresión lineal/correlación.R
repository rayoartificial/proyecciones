library("readxl")
library(dplyr)
library(ggplot2)

set.seed(0)

setwd("D:/Documentos/Rayo Artificial/Análisis de datos/Proyecciones/proyecciones")
ventas <- read_excel("ventas-anuales.xlsx")
ventas %>% summarize(mean(ventas), sd(ventas))
ventas %>% ggplot(aes(ventas)) + geom_histogram(binwidth = 2000)

# Análisis del PBI y las ventas de energía eléctrica

ventas %>% summarize(mean(PBI), sd(PBI))
ventas %>% ggplot(aes(PBI)) + geom_histogram(binwidth=20000)

ventas %>% ggplot(aes(PBI, Ventas)) + geom_point(alpha=0.5)
ventas %>% summarize(cor(PBI, Ventas))

mu_x <- mean(ventas$PBI)
mu_y <- mean(ventas$Ventas)
s_x <- sd(ventas$PBI)
s_y <- sd(ventas$Ventas)

r <- cor(ventas$PBI, ventas$Ventas)
m <-  r * s_y / s_x
b <- mu_y - m*mu_x

ventas %>% 
  ggplot(aes(PBI, Ventas)) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m, color="red")

ventas %>% 
  ggplot(aes(scale(PBI), scale(Ventas))) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r, color="blue")


# Análisis de la población y las ventas de energía eléctrica

ventas %>% summarize(mean(Población), sd(Población))
ventas %>% ggplot(aes(Población)) + geom_histogram(binwidth = 1000)

ventas %>% ggplot(aes(Población, Ventas)) + geom_point(alpha=0.5)
ventas %>% summarize(cor(Población, Ventas))

mu_x <- mean(ventas$Población)
s_x <- sd(ventas$Población)

r <- cor(ventas$Población, ventas$Ventas)
m <-  r * s_y / s_x
b <- mu_y - m*mu_x

ventas %>% 
  ggplot(aes(Población, Ventas)) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m, color="red")

ventas %>% 
  ggplot(aes(scale(Población), scale(Ventas))) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r, color="blue")

