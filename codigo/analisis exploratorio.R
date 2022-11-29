library(corrplot)
library(ggplot2)
library(dplyr)
library(fastDummies)
library(tidyverse)
library(highcharter)

datos_limpios <- read.csv("datos/datos-limpios.csv")

# Relación entre variables

variables_numericas <- datos_limpios %>%
  select_if(is.numeric) %>%
  select("precio_en_usd", everything())

cor <- cor(variables_numericas)

cor

ggcorrplot::ggcorrplot(cor(variables_numericas))

GGally::ggpairs(variables_numericas, 
                aes(alpha = 0.001)) +
  theme(axis.text.x = element_text(angle = 90))


hchart(cor(variables_numericas))

# Vemos que el precio del auto está mas correlacionado con la "edad" del auto y el año producido (que es "lo mismo"). Además el precio esta correlacionado en menor medida con el kilometraje, la capacidad del motor y el numero de fotos

ggplot(datos_limpios) +
  aes(x = fct_infreq(marca)) +
  geom_bar(fill = "#112446") +
  theme_minimal() +
  ylab("Cantidad") +
  xlab("") +
  ggtitle("Cantidad de autos por marca") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(datos_limpios) +
  aes(x = fct_infreq(carroceria)) +
  geom_bar(fill = "#112446") +
  theme_minimal() +
  ylab("Cantidad") +
  xlab("") +
  ggtitle("Cantidad de autos por carrocería") +
  theme(axis.text.x = element_text(angle = 90))


ggplot(datos_limpios, aes(x = reorder(marca, - precio_en_usd), y = precio_en_usd)) + 
  geom_bar(stat = "summary", fun = "mean", fill = "#112446") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("") +
  ylab("Precio promedio") +
  ggtitle("Precio promedio por marca")

ggplot(datos_limpios, aes(x = reorder(carroceria, - precio_en_usd), y = precio_en_usd)) + 
  geom_bar(stat = "summary", fun = "mean", fill = "#112446") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("") +
  ylab("Precio promedio") +
  ggtitle("Precio promedio por carrocería")


ggplot(datos_limpios, aes(x = edad, y = precio_en_usd)) + 
  geom_bar(stat = "summary", fun = "mean", fill = "#112446") +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab("") +
  ylab("Precio promedio") +
  ggtitle("Precio promedio por edad")


datos_limpios <- datos_limpios %>% 
  group_by(marca) %>% 
  mutate(precio_medio_marca = mean(precio_en_usd))

ggplot(datos_limpios) +
  aes(x = edad, y = precio_en_usd, alpha = 0.02) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(carroceria))

