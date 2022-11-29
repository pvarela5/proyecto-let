library(dplyr)
library(tidyverse)

# Importar datos ----

datos <- read.csv("datos/cars.csv")

# Eliminamos las columnas que no son de interés.

datos <- within(datos, {
  feature_0 <- NULL
  feature_1 <- NULL
  feature_2 <- NULL
  feature_3 <- NULL
  feature_4 <- NULL
  feature_5 <- NULL
  feature_6 <- NULL
  feature_7 <- NULL
  feature_8 <- NULL
  feature_9 <- NULL
  engine_has_gas <- NULL
  engine_fuel <- NULL
  has_warranty <- NULL
  state <- NULL
  is_exchangeable <- NULL
  location_region <- NULL
  up_counter <- NULL
})

datos <- datos %>%
  na.omit() %>% # eliminamos valores NA
  mutate(edad = 2020 - year_produced) %>%
  filter(odometer_value > 100, price_usd > 100)

# Agregamos la "edad" del auto, tomaremos en cuenta que al momento de obtener estos datos se esta en el año 2020 (los datos fueron tomados en dic 2019, por lo que aproximamos la año 2020)

# Seleccionamos solo los autos con más de 100 km para asegurarnos de que sea usado y que el precio sea mayor a 100 usd.

colnames(datos) <- c("marca", "modelo", "transmision", "color", "kilometraje", "año_produccion",
                     "tipo_motor", "capacidad_motor", "carroceria", "traccion", "precio_en_usd",
                     "num_fotos", "dias_en_catalogo", "edad")

datos_limpios <- select(datos, -año_produccion)

write_csv(datos_limpios, "datos/datos-limpios.csv")
