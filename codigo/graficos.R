library(ggplot2)
library(dplyr)
library(fastDummies)

## Histograma precios
ggplot(datos_limpios) +
  aes(x = price_usd) +
  geom_histogram(bins = 40L, fill = "#111246") +
  labs(
    x = "$",
    y = "Cantidad",
    title = "Precio de los autos (en US)"
  ) +
  theme_minimal()

ggsave("figuras/precios.png", width = 10, height = 7)

summary(datos_limpios$price_usd)

# Posibles outliers, comentar si se piensa hacer algo con ellos


# Histograma año del auto
ggplot(datos_limpios) +
  aes(x = year_produced) +
  geom_histogram(bins = 35L, fill = "#112446") +
  labs(
    x = "Año",
    y = "Cantidad",
    title = "Año de produccón del auto"
  ) +
  theme_minimal()

ggsave("figuras/años.png", width = 10, height = 7)

summary(datos_limpios$year_produced)

# Posibles outliers, comentar si se piensa hacer algo con ellos


# Marcas con más avisos

marcas <- tail(names(sort(table(datos_limpios$manufacturer_name))), 15)
marcas_mas_publicadas <- filter(datos_limpios, manufacturer_name %in% marcas)


ggplot(marcas_mas_publicadas) +
  aes(x = manufacturer_name) +
  geom_bar(fill = "#112446") +
  labs(
    x = "Marca",
    y = "Cantidad de anuncios",
    title = "Las 15 marcas con más avisos"
  ) +
  theme_minimal()

# Relación entre variables

variables_numericas <- datos_limpios %>% 
  select_if(is.numeric) %>% 
  select("price_usd", everything())

GGally::ggpairs(variables_numericas)
ggsave("figuras/relaciones.png", width = 10, height = 7)



