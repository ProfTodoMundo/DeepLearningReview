
# ===========================================
# Simulación y Análisis de Regresión Logística
# ===========================================
set.seed(123)

# ---- Simulación de datos de cáncer de mama ----
n <- 1000
edad_cancer <- round(rnorm(n, mean = 55, sd = 10))
tamano_tumor <- round(rnorm(n, mean = 30, sd = 10), 1)
progesterona <- round(rnorm(n, mean = 15, sd = 5), 1)

log_odds_cancer <- -4 + 0.03 * edad_cancer + 0.05 * tamano_tumor - 0.1 * progesterona
prob_sobrevida <- 1 / (1 + exp(-log_odds_cancer))
sobrevivio <- rbinom(n, 1, prob_sobrevida)

datos_cancer <- data.frame(
  edad = edad_cancer,
  tamano_tumor = tamano_tumor,
  progesterona = progesterona,
  sobrevivio = sobrevivio
)

# ---- Simulación de datos de enfermedad coronaria ----
edad_chd <- round(rnorm(n, mean = 60, sd = 8))
colesterol <- round(rnorm(n, mean = 220, sd = 30), 1)
presion <- round(rnorm(n, mean = 135, sd = 15), 1)

log_odds_chd <- -6 + 0.04 * edad_chd + 0.03 * colesterol + 0.02 * presion
prob_chd <- 1 / (1 + exp(-log_odds_chd))
enfermedad_corazon <- rbinom(n, 1, prob_chd)

datos_chd <- data.frame(
  edad = edad_chd,
  colesterol = colesterol,
  presion = presion,
  enfermedad = enfermedad_corazon
)

# ---- Ajuste del modelo logístico ----
modelo_cancer <- glm(sobrevivio ~ edad + tamano_tumor + progesterona, 
                     data = datos_cancer, family = binomial)
modelo_chd <- glm(enfermedad ~ edad + colesterol + presion, 
                  data = datos_chd, family = binomial)

summary(modelo_cancer)
summary(modelo_chd)

# ---- Evaluación: odds ratio e IC 95% ----
exp(cbind(OR = coef(modelo_cancer), confint(modelo_cancer)))
exp(cbind(OR = coef(modelo_chd), confint(modelo_chd)))

# ---- Gráfico de la sigmoide para cáncer (vs. edad) ----
library(ggplot2)
datos_cancer$prob <- predict(modelo_cancer, type = "response")

ggplot(datos_cancer, aes(x = edad, y = prob)) +
  geom_point(aes(color = factor(sobrevivio)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Sobrevivir a 5 años vs Edad",
       y = "Probabilidad estimada", color = "Sobrevivió") +
  theme_minimal()

# ---- Gráfico de la sigmoide para CHD (vs. colesterol) ----
datos_chd$prob <- predict(modelo_chd, type = "response")

ggplot(datos_chd, aes(x = colesterol, y = prob)) +
  geom_point(aes(color = factor(enfermedad)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Colesterol",
       y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()

############################################################
# Sección: Optimización vía Gradientes Conjugados (Logística)
############################################################

# Función sigmoide
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

# Gradiente de la función de log-verosimilitud
gradient <- function(X, y, beta) {
  p <- sigmoid(X %*% beta)
  t(X) %*% (p - y)
}

# Producto Hessiano aproximado
hessian_approx <- function(X, beta) {
  p <- sigmoid(X %*% beta)
  W <- diag(as.vector(p * (1 - p)))
  t(X) %*% W %*% X
}

# Método de gradiente conjugado (versión simplificada)
conjugate_gradient <- function(X, y, tol = 1e-6, max_iter = 1000) {
  beta <- matrix(0, ncol(X), 1)
  r <- -gradient(X, y, beta)
  d <- r
  iter <- 0
  
  while (sqrt(sum(r^2)) > tol && iter < max_iter) {
    Hd <- hessian_approx(X, beta) %*% d
    alpha <- as.numeric(t(r) %*% r / (t(d) %*% Hd))
    beta <- beta + alpha * d
    r_new <- r - alpha * Hd
    beta_new <- beta
    
    if (sqrt(sum(r_new^2)) < tol) break
    
    beta <- beta_new
    beta_new <- NULL
    
    beta_r <- t(r_new) %*% r_new / t(r) %*% r
    d <- r_new + as.numeric(beta_r) * d
    r <- r_new
    iter <- iter + 1
  }
  return(list(beta = beta, iter = iter))
}

# Aplicación a los datos simulados de cáncer
X_cancer <- as.matrix(cbind(1, cancer$age, cancer$size))
y_cancer <- as.matrix(cancer$outcome)
cg_cancer <- conjugate_gradient(X_cancer, y_cancer)
print("Coeficientes estimados (cáncer):")
print(cg_cancer$beta)

# Aplicación a los datos simulados de enfermedad cardíaca
X_chd <- as.matrix(cbind(1, chd$age, chd$cholesterol))
y_chd <- as.matrix(chd$outcome)
cg_chd <- conjugate_gradient(X_chd, y_chd)
print("Coeficientes estimados (CHD):")
print(cg_chd$beta)
