
# Simulación de datos para enfermedad coronaria (CHD)

set.seed(123)
n <- 200

# Simulación de variables
edad <- rnorm(n, mean = 55, sd = 10)
colesterol <- rnorm(n, mean = 220, sd = 30)
presion_sistolica <- rnorm(n, mean = 130, sd = 15)
fumar <- rbinom(n, 1, prob = 0.3)
diabetes <- rbinom(n, 1, prob = 0.2)

# Simular log-odds con coeficientes establecidos manualmente
log_odds <- -5 + 0.04 * edad + 0.03 * colesterol + 0.05 * presion_sistolica + 0.8 * fumar + 1.2 * diabetes
probabilidades <- 1 / (1 + exp(-log_odds))
CHD <- rbinom(n, 1, prob = probabilidades)

# Data frame
datos_chd <- data.frame(
  edad, colesterol, presion_sistolica, fumar, diabetes, CHD
)

# Ajuste con glm
modelo_glm <- glm(CHD ~ edad + colesterol + presion_sistolica + fumar + diabetes, 
                  data = datos_chd, family = binomial)

summary(modelo_glm)

# Comparar coeficientes reales y estimados
coef_simulados <- c("(Intercept)" = -5, edad = 0.04, colesterol = 0.03, presion_sistolica = 0.05,
                    fumar = 0.8, diabetes = 1.2)
coef_estimados <- coef(modelo_glm)

comparacion <- data.frame(Simulado = coef_simulados[names(coef_estimados)],
                          Estimado = round(coef_estimados, 3))
print(comparacion)

# Visualizar la sigmoide respecto a la edad
library(ggplot2)
edad_seq <- seq(min(edad), max(edad), length.out = 100)
pred_data <- data.frame(
  edad = edad_seq,
  colesterol = mean(colesterol),
  presion_sistolica = mean(presion_sistolica),
  fumar = 0,
  diabetes = 0
)
pred_data$prob <- predict(modelo_glm, newdata = pred_data, type = "response")

ggplot(datos_chd, aes(x = edad, y = CHD)) +
  geom_jitter(width = 0.5, height = 0.05, alpha = 0.4) +
  geom_line(data = pred_data, aes(x = edad, y = prob), color = "blue", size = 1) +
  labs(title = "Curva Sigmoide: Probabilidad de CHD vs Edad", y = "Probabilidad estimada", x = "Edad")
