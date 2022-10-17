# Importar datos ----

datos <- read.csv("datos/cars.csv")

# Eliminamos las columnas que no son de interés.

datos <- within(datos, {
  drivetrain <- NULL
  engine_capacity <- NULL
  engine_has_gas <- NULL
  engine_fuel <- NULL
  state <- NULL
  body_type <- NULL
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
  has_warranty <- NULL
  is_exchangeable <- NULL
  location_region <- NULL
  up_counter <- NULL 
})

# Seleccionamos solo los autos con más de 100 km para asegurarnos de que sea usado.

datos <- datos %>% filter(odometer_value > 100)

