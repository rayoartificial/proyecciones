library("readxl")
library(dplyr)
library(ggplot2)

set.seed(0)

setwd("D:/Documentos/Rayo Artificial/Análisis de datos/Proyecciones/proyecciones")
ventas <- read_excel("ventas-anuales.xlsx")
datos_proy <- read_excel("ventas-anuales.xlsx", sheet = "2024-2029")

# Análisis del PBI y la ventas
pbi_model <- lm(Ventas ~ PBI, data = ventas)
pbi_model
summary(pbi_model)

pbi_proy <- predict(pbi_model, interval = c("confidence"), level = 0.95)
pbi_data <- as_tibble(pbi_proy) %>% bind_cols(PBI = ventas$PBI)

ggplot(pbi_data, aes(x=PBI, y=fit)) +
  geom_line(linewidth=0.5) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2, fill="black") +
  geom_point(data = ventas, aes(x = PBI, y = Ventas), color="black", size=2) +
  theme_minimal() +
  theme(plot.background = element_rect(color="white", fill="white"),
        legend.position = "none",)


# Análisis de la población y la ventas
pob_model <- lm(Ventas ~ Población, data = ventas)
pob_model
summary(pob_model)

pob_proy <- predict(pob_model, interval = c("confidence"), level = 0.95)
pob_data <- as_tibble(pob_proy) %>% bind_cols(Población = ventas$Población)

ggplot(pob_data, aes(x=Población, y=fit)) +
  geom_line(linewidth=0.5) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2, fill="black") +
  geom_point(data = ventas, aes(x = Población, y = Ventas), color="black", size=2) +
  theme_minimal() +
  theme(plot.background = element_rect(color="white", fill="white"),
        legend.position = "none",)

