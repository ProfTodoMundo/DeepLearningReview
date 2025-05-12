
# Simulación de datos de cáncer de mama
set.seed(123)
n <- 200
edad <- round(runif(n, 30, 80))
tamano_tumor <- round(rnorm(n, mean = 25, sd = 10), 1)
progesterona <- round(rnorm(n, mean = 15, sd = 5), 1)

# Coeficientes simulados para cáncer
log_odds <- -4 + 0.03 * edad + 0.05 * tamano_tumor - 0.1 * progesterona
probabilidad_cancer <- 1 / (1 + exp(-log_odds))
cancer <- rbinom(n, 1, probabilidad_cancer)

datos_cancer <- data.frame(edad, tamano_tumor, progesterona, cancer)

# Ajuste de modelo con glm
modelo_cancer <- glm(cancer ~ edad + tamano_tumor + progesterona, family = binomial, data = datos_cancer)
summary(modelo_cancer)

# Gráfico para visualización
library(ggplot2)
ggplot(datos_cancer, aes(x = edad, y = probabilidad_cancer, color = factor(cancer))) +
  geom_point(alpha = 0.6) +
  stat_function(fun = function(x) 1 / (1 + exp(-(coef(modelo_cancer)[1] + coef(modelo_cancer)[2]*x +
                                                   coef(modelo_cancer)[3]*mean(tamano_tumor) +
                                                   coef(modelo_cancer)[4]*mean(progesterona)))), 
                color = "black", linetype = "dashed") +
  labs(title = "Regresión logística: Cáncer de mama", y = "Probabilidad estimada", color = "Diagnóstico")

# Simulación de datos para enfermedad cardíaca
set.seed(456)
n <- 200
edad_chd <- round(runif(n, 30, 80))
colesterol <- round(rnorm(n, mean = 200, sd = 30), 1)

# Coeficientes simulados para CHD
log_odds_chd <- -6 + 0.04 * edad_chd + 0.02 * colesterol
probabilidad_chd <- 1 / (1 + exp(-log_odds_chd))
chd <- rbinom(n, 1, probabilidad_chd)

datos_chd <- data.frame(edad = edad_chd, colesterol, chd)

# Ajuste de modelo con glm
modelo_chd <- glm(chd ~ edad + colesterol, family = binomial, data = datos_chd)
summary(modelo_chd)

# Gráfico para visualización
ggplot(datos_chd, aes(x = edad, y = probabilidad_chd, color = factor(chd))) +
  geom_point(alpha = 0.6) +
  stat_function(fun = function(x) 1 / (1 + exp(-(coef(modelo_chd)[1] + coef(modelo_chd)[2]*x +
                                                   coef(modelo_chd)[3]*mean(colesterol)))), 
                color = "black", linetype = "dashed") +
  labs(title = "Regresión logística: Enfermedad cardíaca", y = "Probabilidad estimada", color = "Diagnóstico")
