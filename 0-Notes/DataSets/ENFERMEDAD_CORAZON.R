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






##---- Aplicación a los datos simulados de enfermedad cardíaca ----
datos_chd$edad
datos_chd$colesterol
datos_chd$enfermedad
X_chd <- as.matrix(cbind(1, datos_chd$edad, datos_chd$colesterol))
y_chd <- as.matrix(datos_chd$enfermedad)
cg_chd <- conjugate_gradient(X_chd, y_chd)
print("Coeficientes estimados (CHD):")
print(cg_chd$beta)
