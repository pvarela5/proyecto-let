library(tseries)
require(GGally)
library(lmtest)
library(dplyr)
require(MASS)
library(tidyr) 
library(readxl)
library(car)
library(dplyr)
require(corrplot)
library(tidyverse)

datos_limpios <- read.csv("datos/datos-limpios.csv")


# Sin quitar outliers ------------------------------------------------------------

# DEFINIMOS PRIMERO
modelo_nulo <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - carroceria - traccion, datos_limpios)
summary(modelo_nulo)

modelo_full <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - carroceria - traccion, datos_limpios)
summary(modelo_full)

## AIC ---------------

back_aic <- step(modelo_full, direction = "backward", k = 2)
summary(back_aic)

forw_aic <- step(modelo_nulo, scope = formula(modelo_full), direction = "forward", k = 2)
summary(forw_aic)

step_aic <- step(modelo_nulo, scope = formula(modelo_full), direction = "both", k = 2)
summary(step_aic)

## BIC------------

back_bic <- step(modelo_full, direction = "backward", k = log(nrow(datos_limpios)))

forw_bic <- step(modelo_nulo, scope = formula(modelo_full), direction = "forward", k = log(nrow(datos_limpios)))

step_bic <- step(modelo_nulo, scope = formula(modelo_full), direction = "both", k = log(nrow(datos_limpios)))

## MULTICOLINEALIDAD ----------

corrplot(cor(datos_limpios %>% dplyr::select(where(is.numeric))))

# usamos el valor FIV para apreciar si es que existe multicolinealidad

vif(modelo_full)

# notamos que no existe problema de colinealidad ya que todos los valores son inferiores a 10.



# Quitando outliers -------------------------------------------------------

## SUV ----------

base_suv <- datos_limpios %>% filter(carroceria == "suv")
base_suv <- base_suv %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_suv <- cor(base_suv %>% dplyr::select(where(is.numeric)))
correlacion_suv[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:

modelo_suv <- lm(precio_en_usd ~ edad, data = base_suv)
summary(modelo_suv)
plot(precio_en_usd ~ edad, data = base_suv, col = "blue", main = "Relacion Precio vs Año de produccion de los suv", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_suv, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_suv <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_suv)

modelo_full_suv <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_suv)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_suv <- step(modelo_nulo_suv, scope = formula(modelo_full_suv), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_suv)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_suv)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_suv$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_suv)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_suv1 <- base_suv[-palanca_outliers, ]


## MINIVAN -------

base_minivan <- datos_limpios %>% filter(carroceria == "minivan")
base_minivan <- base_minivan %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_minivan <- cor(base_minivan %>% dplyr::select(where(is.numeric)))
correlacion_minivan[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:


modelo_minivan <- lm(precio_en_usd ~ edad, data = base_minivan)
summary(modelo_minivan)
plot(precio_en_usd ~ edad, data = base_minivan, col = "blue", main = "Relacion Precio vs Año de produccion de los MINIVAN", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_minivan, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_minivan <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_minivan)

modelo_full_minivan <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_minivan)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_minivan <- step(modelo_nulo_minivan, scope = formula(modelo_full_minivan), direction = "both", k = 2)
summary(modelo_minivan)
summary(step_aic_minivan)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_minivan)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_minivan$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_minivan)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_minivan1 <- base_minivan[-palanca_outliers, ]

## CABRIOLET -----------

base_cabriolet <- datos_limpios %>% filter(carroceria == "cabriolet")
base_cabriolet <- base_cabriolet %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_cabriolet <- cor(base_cabriolet %>% dplyr::select(where(is.numeric)))
correlacion_cabriolet[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:


modelo_cabriolet <- lm(precio_en_usd ~ edad, data = base_cabriolet)
summary(modelo_cabriolet)
plot(precio_en_usd ~ edad, data = base_cabriolet, col = "blue", main = "Relacion Precio vs Año de produccion de los cabriolet", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_cabriolet, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_cabriolet <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_cabriolet)

modelo_full_cabriolet <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_cabriolet)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_cabriolet <- step(modelo_nulo_cabriolet, scope = formula(modelo_full_cabriolet), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_cabriolet)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_cabriolet)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_cabriolet$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_cabriolet)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_cabriolet1 <- base_cabriolet[-palanca_outliers, ]


## COUPE ------------

base_coupe <- datos_limpios %>% filter(carroceria == "coupe")
base_coupe <- base_coupe %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_coupe <- cor(base_coupe %>% dplyr::select(where(is.numeric)))
correlacion_coupe[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:


modelo_coupe <- lm(precio_en_usd ~ edad, data = base_coupe)
summary(modelo_coupe)
plot(precio_en_usd ~ edad, data = base_coupe, col = "blue", main = "Relacion Precio vs Año de produccion de los coupe", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_coupe, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_coupe <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_coupe)

modelo_full_coupe <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_coupe)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_coupe <- step(modelo_nulo_coupe, scope = formula(modelo_full_coupe), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_coupe)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_coupe)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_coupe$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_coupe)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_coupe1 <- base_coupe[-palanca_outliers, ]




## hatchback -----------

base_hatchback <- datos_limpios %>% filter(carroceria == "hatchback")
base_hatchback <- base_hatchback %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_hatchback <- cor(base_hatchback %>% dplyr::select(where(is.numeric)))
correlacion_hatchback[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:


modelo_hatchback <- lm(precio_en_usd ~ edad, data = base_hatchback)
summary(modelo_hatchback)
plot(precio_en_usd ~ edad, data = base_hatchback, col = "blue", main = "Relacion Precio vs Año de produccion de los hatchback", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_hatchback, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_hatchback <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_hatchback)

modelo_full_hatchback <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_hatchback)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_hatchback <- step(modelo_nulo_hatchback, scope = formula(modelo_full_hatchback), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_hatchback)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_hatchback)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_hatchback$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_hatchback)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_hatchback1 <- base_hatchback[-palanca_outliers, ]

## MINIBUS ------------------

base_minibus <- datos_limpios %>% filter(carroceria == "minibus")
base_minibus <- base_minibus %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_minibus <- cor(base_minibus %>% dplyr::select(where(is.numeric)))
correlacion_minibus[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:


modelo_minibus <- lm(precio_en_usd ~ edad, data = base_minibus)
summary(modelo_minibus)
plot(precio_en_usd ~ edad, data = base_minibus, col = "blue", main = "Relacion Precio vs Año de produccion de los minibus", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_minibus, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_minibus <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_minibus)

modelo_full_minibus <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_minibus)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_minibus <- step(modelo_nulo_minibus, scope = formula(modelo_full_minibus), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_minibus)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_minibus)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_minibus$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_minibus)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_minibus1 <- base_minibus[-palanca_outliers, ]

## MINIVAN --------------------

base_minivan <- datos_limpios %>% filter(carroceria == "minivan")
base_minivan <- base_minivan %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_minivan <- cor(base_minivan %>% dplyr::select(where(is.numeric)))
correlacion_minivan[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:


modelo_minivan <- lm(precio_en_usd ~ edad, data = base_minivan)
summary(modelo_minivan)
plot(precio_en_usd ~ edad, data = base_minivan, col = "blue", main = "Relacion Precio vs Año de produccion de los minivan", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_minivan, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_minivan <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_minivan)

modelo_full_minivan <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_minivan)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_minivan <- step(modelo_nulo_minivan, scope = formula(modelo_full_minivan), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_minivan)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_minivan)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_minivan$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_minivan)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_minivan1 <- base_minivan[-palanca_outliers, ]

## PICKUP ---------------------

base_pickup <- datos_limpios %>% filter(carroceria == "pickup")
base_pickup <- base_pickup %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_pickup <- cor(base_pickup %>% dplyr::select(where(is.numeric)))
correlacion_pickup[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:


modelo_pickup <- lm(precio_en_usd ~ edad, data = base_pickup)
summary(modelo_pickup)
plot(precio_en_usd ~ edad, data = base_pickup, col = "blue", main = "Relacion Precio vs Año de produccion de los pickup", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_pickup, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_pickup <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_pickup)

modelo_full_pickup <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_pickup)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_pickup <- step(modelo_nulo_pickup, scope = formula(modelo_full_pickup), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_pickup)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_pickup)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_pickup$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_pickup)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_pickup1 <- base_pickup[-palanca_outliers, ]


## SEDAN ------------------

base_sedan <- datos_limpios %>% filter(carroceria == "sedan")
base_sedan <- base_sedan %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_sedan <- cor(base_sedan %>% dplyr::select(where(is.numeric)))
correlacion_sedan[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:

modelo_sedan <- lm(precio_en_usd ~ edad, data = base_sedan)
summary(modelo_sedan)
plot(precio_en_usd ~ edad, data = base_sedan, col = "blue", main = "Relacion Precio vs Año de produccion de los sedan", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_sedan, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_sedan <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_sedan)

modelo_full_sedan <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_sedan)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_sedan <- step(modelo_nulo_sedan, scope = formula(modelo_full_sedan), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_sedan)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_sedan)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_sedan$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_sedan)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_sedan1 <- base_sedan[-palanca_outliers, ]

## UNIVERSAL -------------

base_universal <- datos_limpios %>% filter(carroceria == "universal")
base_universal <- base_universal %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_universal <- cor(base_universal %>% dplyr::select(where(is.numeric)))
correlacion_universal[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:


modelo_universal <- lm(precio_en_usd ~ edad, data = base_universal)
summary(modelo_universal)
plot(precio_en_usd ~ edad, data = base_universal, col = "blue", main = "Relacion Precio vs Año de produccion de los universal", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_universal, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_universal <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_universal)

modelo_full_universal <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_universal)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_universal <- step(modelo_nulo_universal, scope = formula(modelo_full_universal), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_universal)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_universal)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_universal$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_universal)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_universal1 <- base_universal[-palanca_outliers, ]


## VAN --------------

base_van <- datos_limpios %>% filter(carroceria == "van")
base_van <- base_van %>% dplyr::select(-carroceria)

# veamos las correlaciones con esta base mas reducida

correlacion_van <- cor(base_van %>% dplyr::select(where(is.numeric)))
correlacion_van[, 3]

# Vemos que la variable que mejor se correlaciona con nuestra variable de insteres(PRECIO) seria la edad:


modelo_van <- lm(precio_en_usd ~ edad, data = base_van)
summary(modelo_van)
plot(precio_en_usd ~ edad, data = base_van, col = "blue", main = "Relacion Precio vs Año de produccion de los van", xlab = "Edad Vehiculo", ylab = "Precio")
abline(modelo_van, col = "red")

# METODO PARA ENCONTRAR EL MEJOR MODELO

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_van <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_van)

modelo_full_van <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_van)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_van <- step(modelo_nulo_van, scope = formula(modelo_full_van), direction = "both", k = 2)
summary(modelo_full)
summary(step_aic_van)

# PUNTOS PALANCA EN NUESTRA BD

X <- model.matrix(modelo_full_van)
H <- X %*% solve(t(X) %*% X) %*% t(X)
# Los h_ii son los elementos de la diagonal

hii <- diag(H)

# Ahora vemos cuales valores son mayoresa a 2p/n para denominarse puntos palanca

p <- length(modelo_full_van$coefficients)
n <- length(hii)

# veamos graficamente si existen

plot(hii, pch = 19, xlab = NULL, ylab = "h_ii", main = "Puntos Palanca")
abline(h = 2 * p / n, lwd = 2, lty = 2, col = "red") # notamos varios puntos palancas

# de manera practica

pts_palanca <- which(hii > 2 * p / n)

# Ahora vemos outliers con los residuos studentizados

alpha <- 0.05 # significancia
t_alpha <- qt(1 - alpha / 2, n - p - 1)

# trabajaremos con unicamente los Residuos studentizados externamente
res_ext <- rstudent(step_aic_van)

# veamos graficamente

plot(res_ext, pch = 20, main = "Residuos Studentizados", xlab = "", ylab = "residuos ext. stu.")
abline(h = c(-t_alpha, t_alpha), lty = 2, lwd = 2, col = "red") # notamos varios puntos fuera de nuestros intervalos

# Identificar los outliers con grandes residuos:

Out <- which(abs(res_ext) > t_alpha)

palanca_outliers <- pts_palanca[pts_palanca %in% Out]
palanca_outliers <- unique(palanca_outliers, incomparables = F)
palanca_outliers

base_van1 <- base_van[-palanca_outliers, ]



# Agrupar base de datos ---------------------------------------------------

base_final <- c()
base_final <- rbind(base_cabriolet1, base_coupe1, base_hatchback1, base_minibus1, base_minivan1, base_pickup1, base_van1, base_universal1, base_sedan1, base_suv1)

write_csv(base_final, "datos/datos-sin-outliers.csv")

# METODO STEPWISE PARA EL MODELO FINAL ------------------------------------

# DEFINIMOS PRIMERO NUESTRO MODELOS NULO Y COMPLETO

modelo_nulo_final <- lm(precio_en_usd ~ 1 - marca - modelo - transmision - color - tipo_motor - traccion, data = base_final)

modelo_full_final <- lm(precio_en_usd ~ . - marca - modelo - transmision - color - tipo_motor - traccion, data = base_final)

# EMPEZAMOS CON EL METODO STEPWISE CON EL CRITERIO AIC

step_aic_final <- step(modelo_nulo_final, scope = formula(modelo_full_final), direction = "both", k = 2)

summary(step_aic)
summary(step_aic_final)