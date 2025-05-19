#---- 0. SEMILLA INICIAL Y LIBRERIAS---- 
set.seed(123)
##---- Librerias ----
library(pROC)
library(ggplot2)
library(broom)
## ---- Simulación de datos de enfermedad coronaria ----
edad_chd_1 <- round(rnorm(n, mean = 60, sd = 8))
colesterol_1 <- round(rnorm(n, mean = 220, sd = 30), 1)
presion_1 <- round(rnorm(n, mean = 135, sd = 15), 1)
log_odds_chd_1 <- -6 + 0.04 * edad_chd_1 + 0.03 * colesterol_1 + 0.02 * presion_1
prob_chd_1 <- 1 / (1 + exp(-log_odds_chd_1))
enfermedad_corazon_1 <- rbinom(n, 1, prob_chd_1)
datos_chd_1 <- data.frame(
  edad = edad_chd_1,
  colesterol = colesterol_1,
  presion = presion_1,
  enfermedad = enfermedad_corazon_1)
exp(cbind(OR = coef(modelo_chd_1), confint(modelo_chd_1)))
## ---- Gráfico de la sigmoide para CHD (vs. colesterol) ----
datos_chd_1$prob <- predict(modelo_chd_1, type = "response")
ggplot(datos_chd_1, aes(x = colesterol, y = prob)) +
  geom_point(aes(color = factor(enfermedad)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Colesterol",
       y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()
modelo_chd_1 <- glm(enfermedad ~ edad + colesterol + presion, 
                    data = datos_chd_1, family = binomial)
summary(modelo_chd_1)


## ---- Simulación de datos de enfermedad coronaria ----
n_4 <- 1000
edad_chd_4 <- round(rnorm(n_4, mean = 60, sd = 8))
colesterol_4 <- round(rnorm(n_4, mean = 220, sd = 30), 1)
presion_4 <- round(rnorm(n_4, mean = 135, sd = 15), 1)
log_odds_chd_4 <- -6 + 0.04 * edad_chd_4 +
  0.03 * colesterol_4 + 0.02 * presion_4
prob_chd_4 <- 1 / (1 + exp(-log_odds_chd_4))
enfermedad_corazon_4 <- rbinom(n_4, 1, prob_chd_4)
datos_chd_4 <- data.frame(
  edad = edad_chd_4,
  colesterol = colesterol_4,
  presion = presion_4,
  enfermedad = enfermedad_corazon_4
)
modelo_chd_4 <- glm(enfermedad ~ edad + colesterol + presion, 
                    data = datos_chd_4, family = binomial)
summary(modelo_chd_4)
exp(cbind(OR = coef(modelo_chd_4), confint(modelo_chd_4)))
## ---- Gráfico de la sigmoide para CHD (vs. colesterol) ----
datos_chd_4$prob <- predict(modelo_chd_4, type = "response")
ggplot(datos_chd_4, aes(x = colesterol, y = prob)) +
  geom_point(aes(color = factor(enfermedad)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Colesterol",
       y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()

n_5 <- 200

# Simulación de datos: CHD
edad_chd_5 <- rnorm(n_5, mean = 60, sd = 12)
presion_chd_5 <- rnorm(n_5, mean = 140, sd = 20)
x_chd_5 <- cbind(1, edad_chd_5, presion_chd_5)
beta_chd_5 <- c(-6, 0.05, 0.03)
logit_chd_5 <- x_chd_5 %*% beta_chd_5
p_chd_5 <- 1 / (1 + exp(-logit_chd_5))
y_chd_5 <- rbinom(n_5, 1, p_chd_5)

# Simulación de datos para enfermedad cardíaca (CHD)
set.seed(123)
n_6 <- 200
AGE_6 <- rnorm(n_6, mean = 55, sd = 10)
CHOL_6 <- rnorm(n_6, mean = 220, sd = 30)
SMOKING_6 <- rbinom(n_6, 1, prob = 0.4)

# Simulación del resultado binario CHD
lin_pred_6 <- -6 + 0.05 * AGE_6 + 0.03 * CHOL_6 + 1.5 * SMOKING_6
prob_CHD_6 <- 1 / (1 + exp(-lin_pred_6))
CHD_6 <- rbinom(n_6, 1, prob_CHD_6)

data_chd_6 <- data.frame(AGE_6, CHOL_6, SMOKING_6, CHD_6)

# Ajuste del modelo logístico
modelo_chd_6 <- glm(CHD_6 ~ AGE_6 + CHOL_6 + SMOKING_6,
                    data = data_chd_6, family = binomial)

# Predicción del valor lineal y la probabilidad
xbeta_6 <- predict(modelo_chd_6, type = "link")
probabilidades_6 <- predict(modelo_chd_6, type = "response")

# Gráfica de la sigmoide con clasificación de puntos
x_vals_6 <- seq(min(xbeta_6) - 1, max(xbeta_6) + 1, length.out = 500)
y_vals_6 <- 1 / (1 + exp(-x_vals_6))

# Plot
plot(x_vals_6, y_vals_6, type = "l", lwd = 2, col = "blue",
     xlab = expression(X*beta), ylab = "Probabilidad", main = "Curva sigmoide con clasificación")
abline(h = 0.5, lty = 2, col = "red")
abline(v = 0, lty = 2, col = "gray")

# Puntos observados
points(xbeta_6[data_chd_6$CHD_6 == 0], data_chd_6$CHD_6[data_chd_6$CHD_6 == 0], 
       col = "darkred", pch = 4)
points(xbeta_6[data_chd_6$CHD_6 == 1], data_chd_6$CHD_6[data_chd_6$CHD_6 == 1], 
       col = "darkgreen", pch = 16)

# Leyenda
legend("bottomright", legend = c("Sigmoide", "CHD = 0", "CHD = 1"),
       col = c("blue", "darkred", "darkgreen"), pch = c(NA, 4, 16), lty = c(1, NA, NA), lwd = c(2, NA, NA))

#---- 6. DATOS DE ENFERMEDADES DEL CORAZON ----
n_6 <- 500
edad_6 <- round(runif(n_6, 35, 80))                      # Edad entre 35 y 80
psistolica_6 <- round(rnorm(n_6, 130, 20))               # Presión sistólica
colesterol_6 <- round(rnorm(n_6, 200, 40))               # Colesterol total
diabetes_6 <- rbinom(n_6, 1, 0.2)                        # Presencia de diabetes (20%)
obesidad_6 <- rbinom(n_6, 1, 0.3)                        # Obesidad (30%)
tabaco_6 <- rbinom(n_6, 1, 0.25)                         # Fumador (25%)
##---- INICIALIZACION DE PARAMETROS DEL MODELO ----
beta_0 <- -6;      beta_edad <- 0.04; beta_psis <- 0.02; 
beta_col <- 0.01;  beta_diab <- 0.7;  beta_obes <- 0.5; beta_taba <- 0.6;
###---- MODELO LOGIT----
log_odds_6 <- beta_0 + beta_edad * edad_6 + beta_psis * psistolica_6 + 
  beta_col * colesterol_6 + beta_diab * diabetes_6 + 
  beta_obes * obesidad_6 + beta_taba * tabaco_6
prob_chd_6 <- 1 / (1 + exp(-log_odds_6))
CHD_6 <- rbinom(n_6, 1, prob_chd_6)
###---- CREACION DATA FRAME ----
datos_chd_6 <- data.frame(
  edad_6, psistolica_6, colesterol_6, diabetes_6, 
  obesidad_6, tabaco_6, CHD_6)
View(datos_chd_6)
head(datos_chd)
##---- CREACION DE ARCHIVO .csv ---- 
write.csv(datos_chd_6, "datos_simulados_CHD_6.csv", row.names = FALSE)
#---- 7. Simulación de datos para enfermedad cardíaca (CHD) ----
set.seed(123)
n_7 <- 200
AGE_7 <- rnorm(n_7, mean = 55, sd = 10)
CHOL_7 <- rnorm(n_7, mean = 220, sd = 30)
SMOKING_7 <- rbinom(n_7, 1, prob = 0.4)
##---- Simulación del resultado binario CHD ----
lin_pred_7 <- -6 + 0.05 * AGE_7 + 0.03 * CHOL_7 + 1.5 * SMOKING_7
prob_CHD_7 <- 1 / (1 + exp(-lin_pred_7))
CHD_7 <- rbinom(n_7, 1, prob_CHD_7)
data_chd_7 <- data.frame(AGE_7, CHOL_7, SMOKING_7, CHD_7)
# Ajuste del modelo logístico
modelo_chd_7 <- glm(CHD_7 ~ AGE_7 + CHOL_7 + SMOKING_7, data = data_chd_7, family = binomial)
# Predicción del valor lineal y la probabilidad
xbeta_7 <- predict(modelo_chd_7, type = "link")
probabilidades_7 <- predict(modelo_chd_7, type = "response")
# Gráfica de la sigmoide con clasificación de puntos
x_vals_7 <- seq(min(xbeta_7) - 1, max(xbeta_7) + 1, length.out = 500)
y_vals_7 <- 1 / (1 + exp(-x_vals_7))
# Plot
plot(x_vals_7, y_vals_7, type = "l", lwd = 2, col = "blue",
     xlab = expression(X*beta), ylab = "Probabilidad", main = "Curva sigmoide con clasificación")
abline(h = 0.5, lty = 2, col = "red")
abline(v = 0, lty = 2, col = "gray")
# Puntos observados
points(xbeta_7[data_chd_7$CHD_7 == 0], data_chd_7$CHD_7[data_chd_7$CHD_7 == 0], col = "darkred", pch = 4)
points(xbeta_7[data_chd_7$CHD_7 == 1], data_chd_7$CHD_7[data_chd_7$CHD_7 == 1], col = "darkgreen", pch = 16)
# Leyenda
legend("bottomright", legend = c("Sigmoide", "CHD = 0", "CHD = 1"),
       col = c("blue", "darkred", "darkgreen"), pch = c(NA, 4, 16), lty = c(1, NA, NA), lwd = c(2, NA, NA))

#----  8. Simulación de datos - Enfermedad Coronaria
edad_chd_8 <- runif(n_8, 30, 80)
b0_chd_8 <- -8
b1_chd_8 <- 0.12
log_odds_chd_8 <- b0_chd_8 + b1_chd_8 * edad_chd_8
prob_chd_8 <- 1 / (1 + exp(-log_odds_chd_8))
diagnostico_chd_8 <- rbinom(n_8, 1, prob_chd_8)
datos_chd_8 <- data.frame(Edad = edad_chd_8,Diagnostico = diagnostico_chd_8)
modelo_chd_8 <- glm(diagnostico_chd_8 ~ Edad_8, family = binomial, data = datos_chd_8)
summary(modelo_chd__8)
##---- Gráfica de la curva sigmoide para CHD ----
edad_seq_8 <- seq(30, 80, length.out = 300)
b0_8 <- coef(modelo_chd_8)[1]
b1_8 <- coef(modelo_chd_8)[2]
prob_pred_8 <- 1 / (1 + exp(-(b0_8 + b1_8 * edad_seq_8)))

datos_sigmoide_8 <- data.frame(Edad = edad_seq_8, Probabilidad = prob_pred_8)

ggplot() +
  geom_point(data = datos_chd_8, aes(x = Edad, y = Diagnostico), alpha = 0.5) +
  geom_line(data = datos_sigmoide_8, aes(x = Edad, y = Probabilidad), color = "blue", size = 1.2) +
  labs(title = "Curva Sigmoide - Probabilidad de Enfermedad Coronaria",
       x = "Edad",
       y = "Probabilidad estimada") +
  theme_minimal()

##---- Aplicación a los datos simulados de enfermedad cardíaca ----
datos_chd$edad
datos_chd$colesterol
datos_chd$enfermedad
X_chd <- as.matrix(cbind(1, datos_chd$edad, datos_chd$colesterol))
y_chd <- as.matrix(datos_chd$enfermedad)
cg_chd <- conjugate_gradient(X_chd, y_chd)
print("Coeficientes estimados (CHD):")
print(cg_chd$beta)
#---- 9. Simulación de datos para enfermedad coronaria (CHD) ----
set.seed(123)
n_9 <- 200
##---- Simulación de variables ----
edad_9 <- rnorm(n_9, mean = 55, sd = 10)
colesterol_9 <- rnorm(n_9, mean = 220, sd = 30)
presion_sistolica_9 <- rnorm(n_9, mean = 130, sd = 15)
fumar_9 <- rbinom(n_9, 1, prob = 0.3)
diabetes_9 <- rbinom(n_9, 1, prob = 0.2)
##---- Simular log-odds con coeficientes establecidos manualmente ----
log_odds_9 <- -5 + 0.04 * edad_9 + 0.03 * colesterol_9 +
  0.05 * presion_sistolica_9 + 0.8 * fumar_9 + 1.2 * diabetes_9
probabilidades_9 <- 1 / (1 + exp(-log_odds_9))
CHD_9 <- rbinom(n, 1, prob = probabilidades_9)
##---- Data frame ----
datos_chd_9 <- data.frame(edad_9, colesterol_9, presion_sistolica_9, fumar_9, diabetes_9, CHD_9)
##---- Ajuste con glm ----
modelo_glm_9 <- glm(CHD_9 ~ edad_9 + colesterol_9 + 
                      presion_sistolica_9 + fumar_9 + diabetes_9, 
                  data = datos_chd_9, family = binomial)
summary(modelo_glm_9)
##---- Comparar coeficientes reales y estimados ----
coef_simulados_9 <- c("(Intercept)" = -5, edad_9 = 0.04, 
                      colestero_9l = 0.03, presion_sistolica_9 = 0.05,
                      fumar_9 = 0.8, diabetes_9 = 1.2)
coef_estimados_9 <- coef(modelo_glm_9)
comparacion_9 <- data.frame(Simulado = coef_simulados_9[names(coef_estimados_9)],
                          Estimado = round(coef_estimados_9, 3))
print(comparacion_9)

##---- Visualizar la sigmoide respecto a la edad ----
library(ggplot2)
edad_seq_9 <- seq(min(edad_9), max(edad_9), length.out = 100)
pred_data_9 <- data.frame(
  edad_9 = edad_seq_9,
  colesterol_9 = mean(colesterol_9),
  presion_sistolica_9 = mean(presion_sistolica_9),
  fumar_9 = 0,
  diabetes_9 = 0
)
pred_data_9$prob <- predict(modelo_glm_9, newdata = pred_data_9, type = "response")
ggplot(datos_chd_9, aes(x = edad_9, y = CHD_9)) +
  geom_jitter(width = 0.5, height = 0.05, alpha = 0.4) +
  geom_line(data = pred_data_9, aes(x = edad_9, y = prob), color = "blue", size = 1) +
  labs(title = "Curva Sigmoide: Probabilidad de CHD vs Edad", y = "Probabilidad estimada", x = "Edad")

#---- 10. Simulación de datos para enfermedad cardíaca ----
set.seed(456)
n_10 <- 200
edad_chd_10 <- round(runif(n_10, 30, 80))
colesterol_10 <- round(rnorm(n_10, mean = 200, sd = 30), 1)
# Coeficientes simulados para CHD
log_odds_chd_10 <- -6 + 0.04 * edad_chd_10 + 0.02 * colesterol_10
probabilidad_chd_10 <- 1 / (1 + exp(-log_odds_chd_10))
chd_10 <- rbinom(n_10, 1, probabilidad_chd_10)
datos_chd_10 <- data.frame(edad = edad_chd_10, colesterol_10, chd_10)
##---- Ajuste de modelo con glm ----
modelo_chd_10 <- glm(chd_10 ~ edad + colesterol_10, 
                     family = binomial, data = datos_chd_10)
summary(modelo_chd_10)
##---- Gráfico para visualización ----
ggplot(datos_chd_10, aes(x = edad, y = probabilidad_chd_10, color = factor(chd_10))) +
  geom_point(alpha = 0.6) +
  stat_function(fun = function(x) 1 / (1 + exp(-(coef(modelo_chd_10)[1] + coef(modelo_chd_10)[2]*x +
                                                   coef(modelo_chd_10)[3]*mean(colesterol_10)))), 
                color = "black", linetype = "dashed") +
  labs(title = "Regresión logística: Enfermedad cardíaca", y = "Probabilidad estimada", color = "Diagnóstico")




