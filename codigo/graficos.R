library(ggplot2)
library(dplyr)




sort(table(datos$manufacturer_name), decreasing=TRUE)[1:15]

ggplot(marcas) +
  aes(x = price_usd) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()


Rcmdr::Commander()

GGally::ggpairs(variables_numericas)

variables_numericas <- datos %>% 
  select_if(is.numeric)

## Histograma precios
ggplot(datos) +
  aes(x = price_usd) +
  geom_histogram(bins = 40L, fill = "#111246") +
  labs(
    x = "$",
    y = "Cantidad",
    title = "Precio de los autos (en US)"
  ) +
  theme_minimal()

summary(datos$price_usd)

# Posibles outliers, comentar si se piensa hacer algo con ellos


# Histograma año del auto
ggplot(datos) +
  aes(x = year_produced) +
  geom_histogram(bins = 35L, fill = "#112446") +
  labs(
    x = "Año",
    y = "Cantidad",
    title = "Año de produccón del auto"
  ) +
  theme_minimal()

summary(datos$year_produced)

# Posibles outliers, comentar si se piensa hacer algo con ellos


# Marcas con más avisos

marcas <- tail(names(sort(table(datos$manufacturer_name))), 15)
marcas_mas_publicadas <- filter(datos, manufacturer_name %in% marcas)


ggplot(marcas_mas_publicadas) +
  aes(x = manufacturer_name) +
  geom_bar(fill = "#112446") +
  labs(
    x = "Marca",
    y = "Cantidad de anuncios",
    title = "Las 15 marcas con más avisos"
  ) +
  theme_minimal()