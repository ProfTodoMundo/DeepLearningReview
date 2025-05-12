
# Simulación y ajuste de modelo logístico para cáncer de mama

# 1. Simular datos
set.seed(123)
n <- 500
edad <- rnorm(n, mean = 55, sd = 10)
tamano_tumor <- rnorm(n, mean = 2.5, sd = 1)
progesterona <- rnorm(n, mean = 15, sd = 5)
HER2 <- rbinom(n, 1, 0.3)
RE <- rbinom(n, 1, 0.6)
RP <- rbinom(n, 1, 0.5)
densidad_mamaria <- rnorm(n, mean = 0.6, sd = 0.2)

# Log-odds simulados con coeficientes reales
log_odds <- -4 + 0.03 * edad + 0.05 * tamano_tumor - 0.1 * progesterona +
            0.8 * HER2 + 0.6 * RE + 0.5 * RP + 0.3 * densidad_mamaria
probabilidad <- 1 / (1 + exp(-log_odds))
cancer <- rbinom(n, 1, probabilidad)

# Construir el data frame
datos <- data.frame(
  edad, tamano_tumor, progesterona, HER2, RE, RP,
  densidad_mamaria, cancer = factor(cancer)
)

# 2. Ajustar modelo logístico con glm()
modelo <- glm(cancer ~ edad + tamano_tumor + progesterona + HER2 + RE + RP + densidad_mamaria,
              data = datos, family = binomial())

# 3. Comparación de coeficientes
coef_verdaderos <- c("(Intercept)" = -4, "edad" = 0.03, "tamano_tumor" = 0.05,
                     "progesterona" = -0.1, "HER2" = 0.8, "RE" = 0.6,
                     "RP" = 0.5, "densidad_mamaria" = 0.3)

coef_estimados <- coef(modelo)
comparacion <- data.frame(
  Coeficiente = names(coef_verdaderos),
  Verdadero = coef_verdaderos,
  Estimado = round(coef_estimados[names(coef_verdaderos)], 4),
  Diferencia = round(coef_estimados[names(coef_verdaderos)] - coef_verdaderos, 4)
)

print(comparacion)

# 4. Visualizar sigmoide
library(ggplot2)

datos$prob_predicha <- predict(modelo, type = "response")

ggplot(datos, aes(x = edad, y = prob_predicha, color = cancer)) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = FALSE, color = "black") +
  labs(title = "Probabilidad predicha vs edad",
       y = "Probabilidad predicha", x = "Edad")
