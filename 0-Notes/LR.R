#----Modelo de Regresión Logística----
##----Datos----
auto_time <- c(52.90, 4.10, 4.10, 56.20, 51.80, 0.20, 27.60, 89.90, 41.50,
               95.00, 99.10, 18.50, 82.00, 8.60, 22.50, 51.40, 81.00, 51.00,
               62.20, 95.10, 41.60)
bus_time <- c(4.4, 28.5, 86.9, 31.6, 20.2, 91.2, 79.7, 2.2, 24.5,
              43.5, 8.4, 84, 38, 1.6, 74.1, 83.8, 19.2, 85, 90.1, 22.2, 91.5)
y <- c(0, 0, 1, 0, 0, 1, 1, 0, 0,
       0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1)
x <- auto_time - bus_time
##---- Ajuste del modelo logístico ----
modelo <- glm(y ~ x, family = binomial)
summary(modelo)
# Visualización de los datos y la curva ajustada
plot(x, y, 
     pch = 19, 
     col = ifelse(y == 1, "blue", "red"),
     main = "Logistic Regression Fit",
     xlab = "x = Auto Time - Bus Time",
     ylab = "P(car)")
curve(predict(modelo, 
              data.frame(x = sort(x)), 
              type = "response"),
      add = TRUE, 
      col = "darkgreen",
      lwd = 2)
legend("bottomright", 
       legend = c("Observado 1", "Observado 0", "Curva logística"),
       col = c("blue", "red", "darkgreen"),
       pch = c(19, 19, NA), 
       lty = c(NA, NA, 1))
#---- Ajuste por mínimos cuadrados no lineales ----
b0_init <- coef(glm(y ~ x, family = binomial))[1]
b1_init <- coef(glm(y ~ x, family = binomial))[2]
modelo_nls <- nls(
  y ~ 1 / (1 + exp(-(b0 + b1 * x))),
  start = list(b0 = b0_init , b1 = b1_init)
)
summary(modelo_nls)
coef(glm(y ~ x, family = binomial))
coef(modelo_nls)
plot(x, y, pch = 19, col = ifelse(y == 1, "blue", "red"),
     main = "Comparación de Métodos de Ajuste",
     xlab = "x = Auto Time - Bus Time", ylab = "P(car)")
x_sorted <- sort(x)
lines(x_sorted,
      predict(glm(y ~ x, 
                  family = binomial), 
              type = "response")[order(x)],
      col = "darkgreen", 
      lwd = 2)
b0_nls <- coef(modelo_nls)["b0"]
b1_nls <- coef(modelo_nls)["b1"]
p_nls <- 1 / (1 + exp(-(b0_nls + b1_nls * x_sorted)))
lines(x_sorted, p_nls, col = "purple", lwd = 2, lty = 2)
legend("bottomleft",
       legend = c("GLM (Verosimilitud)", "NLS (Mínimos Cuadrados)"),
       col = c("darkgreen", "purple"), 
       lty = c(1, 2), 
       lwd = 2,
       cex = 0.5)
#---- Regresion Probit ----
# Tus datos
# Ajuste del modelo PROBIT
modelo_probit <- glm(y ~ x, family = binomial(link = "probit"))
# Resumen
summary(modelo_probit)
# Base del gráfico
plot(x, y,
     pch = 19,
     col = ifelse(y == 1, "blue", "red"),
     main = "Ajuste de Regresión Probit",
     xlab = "x = Auto Time - Bus Time",
     ylab = "P(car)")
# Crear secuencia de valores para trazar la curva
x_seq <- seq(min(x), max(x), 
             length.out = 100)
# Añadir la curva probit
lines(x_seq, 
      predict(modelo_probit,
                     newdata = data.frame(x = x_seq),
                     type = "response"),
      col = "orange", 
      lwd = 2)
# Leyenda
legend("bottomleft",
       legend = c("Observado 1", "Observado 0", "Curva Probit"),
       col = c("blue", "red", "orange"),
       pch = c(19, 19, NA),
       lty = c(NA, NA, 1),
       lwd = c(NA, NA, 2),
       cex = 0.8)
#--- comparativo ----
# Ajustes de los modelos
modelo_logit  <- glm(y ~ x, family = binomial(link = "logit"))
modelo_probit <- glm(y ~ x, family = binomial(link = "probit"))
# Base del gráfico
plot(x, y,
     pch = 19,
     col = ifelse(y == 1, "blue", "red"),
     main = "Comparación: Logit vs Probit",
     xlab = "x = Auto Time - Bus Time",
     ylab = "P(car)",
     ylim = c(0, 1))
# Secuencia para curvas suaves
x_seq <- seq(min(x), max(x), length.out = 100)
# Curva logística (logit)
lines(x_seq, predict(modelo_logit, 
                     newdata = data.frame(x = x_seq), 
                     type = "response"),
      col = "darkgreen", lwd = 2)
# Curva probit
lines(x_seq, predict(modelo_probit, 
                     newdata = data.frame(x = x_seq), 
                     type = "response"),
      col = "orange", 
      lwd = 2,
      lty = 2)
# Leyenda
legend("bottomleft",
       legend = c("Observado 1", "Observado 0", "Logit", "Probit"),
       col = c("blue", "red", "darkgreen", "orange"),
       pch = c(19, 19, NA, NA),
       lty = c(NA, NA, 1, 2),
       lwd = c(NA, NA, 2, 2),
       cex = 0.5)
## ---- Conclusión general ----
# La regresión logística y probit ofrecen estimaciones similares para este conjunto de datos.
# La curva logística (logit) tiende a ser ligeramente más empinada en el centro,
# mientras que la probit es más aplanada en los extremos.
# Ambos modelos son adecuados, pero en contextos con muchas observaciones extremas,
# el probit puede ofrecer un mejor ajuste teórico.

#---- Remuestreo ----
datos <- data.frame(auto_time, bus_time, 
                    x = auto_time - bus_time, 
                    y)
bootstrap_sample <- datos[sample(1:nrow(datos), 
                                 size = 100, 
                                 replace = TRUE), ]
head(bootstrap_sample)
# Ver primeros registros
x_btsp <- bootstrap_sample$x
y_btsp <- bootstrap_sample$y
# Modelo logístico
modelo_btstrap <- glm(y_btsp ~ x_btsp, family = binomial)
summary(modelo_btstrap)
## Visualización
plot(x_btsp, y_btsp, pch = 19, 
     col = ifelse(y_btsp == 1, "blue", "red"),
     main = "Logistic Regression Fit (Bootstap)",
     xlab = "x = Auto Time - Bus Time", 
     ylab = "P(car)")
# Crear una secuencia de x para evaluar la curva suavemente
x_seq <- seq(min(x_btsp), max(x_btsp), length.out = 100)
# Añadir la curva logística estimada con el modelo bootstrap
lines(x_seq, predict(modelo_btstrap, 
                     newdata = data.frame(x_btsp = x_seq),
                     type = "response"),
      col = "darkgreen", 
      lwd = 2)
# Leyenda
legend("bottomleft",
       legend = c("Observado 1", "Observado 0", "Curva logística"),
       col = c("blue", "red", "darkgreen"),
       pch = c(19, 19, NA),
       lty = c(NA, NA, 1),
       lwd = c(NA, NA, 2),
       cex = 0.8)
b0_init <- coef(glm(y ~ x, family = binomial))[1]
b1_init <- coef(glm(y ~ x, family = binomial))[2]
# Ajuste por mínimos cuadrados no lineales
modelo_nls_btsp <- nls(
  y_btsp ~ 1 / (1 + exp(-(b0 + b1 * x_btsp))),
  start = list(b0 = b0_init, b1 = b1_init)
)
summary(modelo_nls_btsp)
coef(glm(y_btsp ~ x_btsp, family = binomial))
coef(modelo_nls_btsp)
plot(x_btsp,
     y_btsp, 
     pch = 19,
     col = ifelse(y_btsp == 1, "blue", "red"),
     main = "Comparación de Métodos de Ajuste",
     xlab = "x = Auto Time - Bus Time", ylab = "P(car)")
x_sorted <- sort(x_btsp)
lines(x_sorted,
      predict(glm(y_btsp ~ x_btsp, family = binomial), 
              type = "response")[order(x_btsp)],
      col = "darkgreen", lwd = 2)
b0_nls <- coef(modelo_nls_btsp)["b0"]
b1_nls <- coef(modelo_nls_btsp)["b1"]
p_nls <- 1 / (1 + exp(-(b0_nls + b1_nls * x_sorted)))
lines(x_sorted, p_nls, col = "purple", lwd = 2, lty = 2)
legend("bottomleft",
       legend = c("GLM (Verosimilitud)", "NLS (Mínimos Cuadrados)"),
       col = c("darkgreen", "purple"), 
       lty = c(1, 2), 
       lwd = 2,
       cex = 0.5)
coef(glm(y_btsp ~ x_btsp, family = binomial))
coef(modelo_nls_btsp)
##---- modelo probit ---
# Ajuste del modelo PROBIT
modelo_probit_btsp <- glm(y_btsp ~ x_btsp,
                     family = binomial(link = "probit"))
# Resumen
summary(modelo_probit_btsp)
# Base del gráfico
plot(x_btsp, y_btsp,
     pch = 19,
     col = ifelse(y_btsp == 1, "blue", "red"),
     main = "Ajuste de Regresión Probit (Bootstrap)",
     xlab = "x = Auto Time - Bus Time",
     ylab = "P(car)")
# Crear secuencia de valores para trazar la curva
x_seq_btsp <- seq(min(x_btsp), max(x_btsp), 
             length.out = 100)
# Añadir la curva probit

lines(x_seq_btsp, 
      predict(modelo_probit_btsp,
              newdata = data.frame(x_btsp = x_seq_btsp),
              type = "response"),
      col = "orange", 
      lwd = 2)

# Leyenda
legend("bottomleft",
       legend = c("Observado 1", "Observado 0", "Curva Probit"),
       col = c("blue", "red", "orange"),
       pch = c(19, 19, NA),
       lty = c(NA, NA, 1),
       lwd = c(NA, NA, 2),
       cex = 0.5)
#--- comparativo ----
# Ajustes de los modelos
modelo_logit_btsp  <- glm(y_btsp ~ x_btsp, 
                          family = binomial(link = "logit"))
modelo_probit_btsp <- glm(y_btsp ~ x_btsp, 
                          family = binomial(link = "probit"))
# Base del gráfico
plot(x_btsp, y_btsp,
     pch = 19,
     col = ifelse(y_btsp == 1, "blue", "red"),
     main = "Comparación: Logit vs Probit (Bootstrap)",
     xlab = "x = Auto Time - Bus Time",
     ylab = "P(car)",
     ylim = c(0, 1))
# Secuencia para curvas suaves
x_seq_btsp <- seq(min(x_btsp), max(x_btsp), length.out = 100)
# Curva logística (logit)
lines(x_seq_btsp, predict(modelo_logit_btsp, 
                     newdata = data.frame(x_btsp = x_seq_btsp), 
                     type = "response"),
      col = "darkgreen", lwd = 2)

# Curva probit
lines(x_seq_btsp, predict(modelo_probit_btsp, 
                     newdata = data.frame(x_btsp = x_seq_btsp), 
                     type = "response"),
      col = "orange", 
      lwd = 2,
      lty = 2)
# Leyenda
legend("bottomleft",
       legend = c("Observado 1", "Observado 0", "Logit", "Probit"),
       col = c("blue", "red", "darkgreen", "orange"),
       pch = c(19, 19, NA, NA),
       lty = c(NA, NA, 1, 2),
       lwd = c(NA, NA, 2, 2),
       cex = 0.5)

