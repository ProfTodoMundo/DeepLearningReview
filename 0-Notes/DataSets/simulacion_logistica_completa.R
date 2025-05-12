
# ========================================
# Simulación de datos para regresión logística
# Incluye datos de cáncer y enfermedad cardíaca
# Cálculo del MLE y gráfica de la sigmoide
# ========================================

# Librerías necesarias
library(ggplot2)
library(dplyr)

# ---------------------------
# Simulación de datos - Cáncer
# ---------------------------
set.seed(123)
n <- 100
edad_cancer <- runif(n, 30, 80)
b0_cancer <- -6
b1_cancer <- 0.1
log_odds_cancer <- b0_cancer + b1_cancer * edad_cancer
prob_cancer <- 1 / (1 + exp(-log_odds_cancer))
diagnostico_cancer <- rbinom(n, 1, prob_cancer)

datos_cancer <- data.frame(
  Edad = edad_cancer,
  Diagnostico = diagnostico_cancer
)

# ---------------------------
# Simulación de datos - Enfermedad Coronaria
# ---------------------------
edad_chd <- runif(n, 30, 80)
b0_chd <- -8
b1_chd <- 0.12
log_odds_chd <- b0_chd + b1_chd * edad_chd
prob_chd <- 1 / (1 + exp(-log_odds_chd))
diagnostico_chd <- rbinom(n, 1, prob_chd)

datos_chd <- data.frame(
  Edad = edad_chd,
  Diagnostico = diagnostico_chd
)

# ---------------------------
# Ajuste del modelo y estimación MLE
# ---------------------------
modelo_cancer <- glm(Diagnostico ~ Edad, family = binomial, data = datos_cancer)
modelo_chd <- glm(Diagnostico ~ Edad, family = binomial, data = datos_chd)

summary(modelo_cancer)
summary(modelo_chd)

# ---------------------------
# Gráfica de la curva sigmoide para CHD
# ---------------------------
edad_seq <- seq(30, 80, length.out = 300)
b0 <- coef(modelo_chd)[1]
b1 <- coef(modelo_chd)[2]
prob_pred <- 1 / (1 + exp(-(b0 + b1 * edad_seq)))

datos_sigmoide <- data.frame(Edad = edad_seq, Probabilidad = prob_pred)

ggplot() +
  geom_point(data = datos_chd, aes(x = Edad, y = Diagnostico), alpha = 0.5) +
  geom_line(data = datos_sigmoide, aes(x = Edad, y = Probabilidad), color = "blue", size = 1.2) +
  labs(title = "Curva Sigmoide - Probabilidad de Enfermedad Coronaria",
       x = "Edad",
       y = "Probabilidad estimada") +
  theme_minimal()
