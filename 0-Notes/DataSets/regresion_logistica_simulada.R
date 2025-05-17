#---- SEMILLA INICIAL Y LIBRERIAS---- 
set.seed(123)
library(ggplot2)
# ---- 1. Simulación de datos de cáncer de mama ----
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
## ---- Simulación de datos de enfermedad coronaria ----
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
## ---- Ajuste del modelo logístico ----
modelo_cancer <- glm(sobrevivio ~ edad + tamano_tumor + progesterona, 
                     data = datos_cancer, family = binomial)
modelo_chd <- glm(enfermedad ~ edad + colesterol + presion, 
                  data = datos_chd, family = binomial)
summary(modelo_cancer)
summary(modelo_chd)
## ---- Evaluación: odds ratio e IC 95% ----
exp(cbind(OR = coef(modelo_cancer), confint(modelo_cancer)))
exp(cbind(OR = coef(modelo_chd), confint(modelo_chd)))
## ---- Gráfico de la sigmoide para cáncer (vs. edad) ----
datos_cancer$prob <- predict(modelo_cancer, type = "response")
ggplot(datos_cancer, aes(x = edad, y = prob)) +
  geom_point(aes(color = factor(sobrevivio)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Sobrevivir a 5 años vs Edad",
       y = "Probabilidad estimada", color = "Sobrevivió") +
  theme_minimal()
## ---- Gráfico de la sigmoide para CHD (vs. colesterol) ----
datos_chd$prob <- predict(modelo_chd, type = "response")
ggplot(datos_chd, aes(x = colesterol, y = prob)) +
  geom_point(aes(color = factor(enfermedad)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Colesterol",
       y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()
#---- 2. Segundo ejemplo de simulacion ----
##---- Librerias ----
library(pROC)
library(ggplot2)
library(broom)
##---- Simulación de datos (igual que antes) ----
set.seed(777)
n <- 1300
age <- round(runif(n, 20, 80))
tumor_size <- round(rnorm(n, 2.5, 1), 1)
marker_A <- round(rnorm(n, 1.0, 0.3), 1)
marker_B <- round(rnorm(n, 1.2, 0.4), 1)
log_odds <- -3.126 + 0.032 * age + 0.732 * tumor_size + 1.348 * marker_A + 0.898 * marker_B
prob <- 1 / (1 + exp(-log_odds))
cancer_type <- rbinom(n, 1, prob)
df <- data.frame(Age = age, TumorSize = tumor_size, MarkerA = marker_A, MarkerB = marker_B, CancerType = cancer_type)
##---- Modelo ----
modelo <- glm(CancerType ~ Age + TumorSize + MarkerA + MarkerB, data = df, family = binomial)
##---- Gráfico 1: Curva ROC ---- 
prob_pred <- predict(modelo, type = "response")
roc_obj <- roc(df$CancerType, prob_pred)
plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC - Modelo cáncer simulado")
abline(a = 0, b = 1, col = "gray", lty = 2)
##---- Gráfico 2: Histograma de probabilidades predichas ----
ggplot(data.frame(prob = prob_pred), aes(x = prob)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "Histograma de probabilidades predichas", x = "Probabilidad estimada", y = "Frecuencia")
##---- Gráfico 3: Efectos de los coeficientes con IC95% ----
coef_data <- tidy(modelo, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
coef_data <- coef_data[coef_data$term != "(Intercept)", ]
ggplot(coef_data, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "darkred") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Odds Ratio con IC95%", x = "Variable", y = "OR (exp(β))") +
  theme_minimal()
#---- 3. NUEVO EJERCICIO DE SIMULACION ----
##---- INICIALIZACION DE VARIABLES ----
set.seed(123)
n <- 500
edad <- round(runif(n, 25, 80))
tamano_tumor <- round(runif(n, 5, 50))
progesterona <- round(runif(n, 1, 100))
HER2 <- rbinom(n, 1, 0.25)
RE <- rbinom(n, 1, 0.6)
RP <- rbinom(n, 1, 0.55)
densidad_mamaria <- sample(1:4, n, replace = TRUE)
##---- Coeficientes simulados----
log_odds <- -4 + 0.03 * edad + 0.05 * tamano_tumor - 0.1 * progesterona +
  0.8 * HER2 + 0.6 * RE + 0.5 * RP + 0.3 * densidad_mamaria
##---- Probabilidad y diagnóstico ----
prob <- 1 / (1 + exp(-log_odds))
diagnostico <- rbinom(n, 1, prob)
##---- Base de datos ----
datos <- data.frame(edad, tamano_tumor, progesterona,
                    HER2, RE, RP, densidad_mamaria, 
                    diagnostico)
##----Modelo de regresión logística (MLE) ----
modelo <- glm(diagnostico ~ ., data = datos, family = binomial)
summary(modelo)
##---- Comparar coeficientes verdaderos vs estimados----
coef_verdaderos <- c(-4, 0.03, 0.05, -0.1, 0.8, 0.6, 0.5, 0.3)
names(coef_verdaderos) <- names(coef(modelo))
comparacion <- data.frame(
  Coef_Verdadero = coef_verdaderos,
  Coef_Estimado = coef(modelo)
)
print(comparacion)
##---- Visualizar con función sigmoide ----
datos$probabilidad_estimada <- predict(modelo, type = "response")
ggplot(datos, aes(x = edad, y = probabilidad_estimada, 
                  color = factor(diagnostico))) +
  geom_point(alpha = 0.6) +
  stat_function(fun = function(x) 1 / (1 + exp(-(-4 + 0.03 * x + 0.05 * 25 - 0.1 * 50))),
                color = "black", linetype = "dashed") +
  labs(title = "Función Sigmoide Estimada vs Real",
       x = "Edad", y = "Probabilidad Estimada",
       color = "Diagnóstico") +
  theme_minimal()
write.csv(datos_cancer, "datos_cancer_simulados3.csv", row.names = FALSE)
View(datos_cancer)
head(datos_cancer)
#---- 4. Simulación y Análisis de Regresión Logística ----
set.seed(123)
library(ggplot2)
##---- Simulación de datos de cáncer de mama ----
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
## ---- Simulación de datos de enfermedad coronaria ----
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
##---- Ajuste del modelo logístico ----
modelo_cancer <- glm(sobrevivio ~ edad + tamano_tumor + progesterona, 
                     data = datos_cancer, family = binomial)
modelo_chd <- glm(enfermedad ~ edad + colesterol + presion, 
                  data = datos_chd, family = binomial)
summary(modelo_cancer)
summary(modelo_chd)
## ---- Evaluación: odds ratio e IC 95% ----
exp(cbind(OR = coef(modelo_cancer), confint(modelo_cancer)))
exp(cbind(OR = coef(modelo_chd), confint(modelo_chd)))
## ---- Gráfico de la sigmoide para cáncer (vs. edad) ----
datos_cancer$prob <- predict(modelo_cancer, type = "response")
ggplot(datos_cancer, aes(x = edad, y = prob)) +
  geom_point(aes(color = factor(sobrevivio)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Sobrevivir a 5 años vs Edad",
       y = "Probabilidad estimada", color = "Sobrevivió") +
  theme_minimal()
## ---- Gráfico de la sigmoide para CHD (vs. colesterol) ----
datos_chd$prob <- predict(modelo_chd, type = "response")
ggplot(datos_chd, aes(x = colesterol, y = prob)) +
  geom_point(aes(color = factor(enfermedad)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Colesterol",
       y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()
#---- 5. Optimización vía Gradientes Conjugados (Logística) ----
##---- Función sigmoide ----
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}
##---- Gradiente de la función de log-verosimilitud ----
gradient <- function(X, y, beta) {
  p <- sigmoid(X %*% beta)
  t(X) %*% (p - y)
}
##---- Producto Hessiano aproximado ----
hessian_approx <- function(X, beta) {
  p <- sigmoid(X %*% beta)
  W <- diag(as.vector(p * (1 - p)))
  t(X) %*% W %*% X
}
##---- Método de gradiente conjugado (versión simplificada) ----
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
##---- Aplicación a los datos simulados de cáncer ----
datos_cancer$edad
datos_cancer$tamano_tumor
datos_cancer$sobrevivio
X_cancer <- as.matrix(cbind(1, datos_cancer$edad, datos_cancer$tamano_tumor))
y_cancer <- as.matrix(datos_cancer$sobrevivio)
cg_cancer <- conjugate_gradient(X_cancer, y_cancer)
print("Coeficientes estimados (cáncer):")
print(cg_cancer$beta)
##---- Aplicación a los datos simulados de enfermedad cardíaca ----
datos_chd$edad
datos_chd$colesterol
datos_chd$enfermedad
X_chd <- as.matrix(cbind(1, datos_chd$edad, datos_chd$colesterol))
y_chd <- as.matrix(datos_chd$enfermedad)
cg_chd <- conjugate_gradient(X_chd, y_chd)
print("Coeficientes estimados (CHD):")
print(cg_chd$beta)
#---- 6. Simulación y estimación MLE para regresión logística ----
set.seed(123)
##---- Simulación de datos: Cáncer ----
n <- 200
edad_cancer <- rnorm(n, mean = 55, sd = 10)
gen_cancer <- rbinom(n, 1, prob = 0.4)
x_cancer <- cbind(1, edad_cancer, gen_cancer)
beta_cancer <- c(-5, 0.08, 1.2)
logit_cancer <- x_cancer %*% beta_cancer
p_cancer <- 1 / (1 + exp(-logit_cancer))
y_cancer <- rbinom(n, 1, p_cancer)

# Simulación de datos: CHD
edad_chd <- rnorm(n, mean = 60, sd = 12)
presion_chd <- rnorm(n, mean = 140, sd = 20)
x_chd <- cbind(1, edad_chd, presion_chd)
beta_chd <- c(-6, 0.05, 0.03)
logit_chd <- x_chd %*% beta_chd
p_chd <- 1 / (1 + exp(-logit_chd))
y_chd <- rbinom(n, 1, p_chd)

# ============================================
# Función de verosimilitud para MLE
# ============================================
log_likelihood <- function(beta, X, y) {
  p <- 1 / (1 + exp(-X %*% beta))
  ll <- sum(y * log(p) + (1 - y) * log(1 - p))
  return(-ll)  # Negativa porque optim minimiza
}

# Gradiente de la log-verosimilitud
log_likelihood_grad <- function(beta, X, y) {
  p <- 1 / (1 + exp(-X %*% beta))
  grad <- -t(X) %*% (y - p)
  return(as.vector(grad))
}

# Estimación MLE para cáncer
mle_cancer <- optim(par = c(0, 0, 0),
                    fn = log_likelihood,
                    gr = log_likelihood_grad,
                    method = "BFGS",
                    X = x_cancer,
                    y = y_cancer,
                    control = list(fnscale = 1))

# Estimación MLE para CHD
mle_chd <- optim(par = c(0, 0, 0),
                 fn = log_likelihood,
                 gr = log_likelihood_grad,
                 method = "BFGS",
                 X = x_chd,
                 y = y_chd,
                 control = list(fnscale = 1))

# Resultados
cat("\n--- Resultados MLE Cáncer ---\n")
print(mle_cancer$par)
cat("\n--- Resultados MLE CHD ---\n")
print(mle_chd$par)


# Simulación de datos para enfermedad cardíaca (CHD)
set.seed(123)
n <- 200
AGE <- rnorm(n, mean = 55, sd = 10)
CHOL <- rnorm(n, mean = 220, sd = 30)
SMOKING <- rbinom(n, 1, prob = 0.4)

# Simulación del resultado binario CHD
lin_pred <- -6 + 0.05 * AGE + 0.03 * CHOL + 1.5 * SMOKING
prob_CHD <- 1 / (1 + exp(-lin_pred))
CHD <- rbinom(n, 1, prob_CHD)

data_chd <- data.frame(AGE, CHOL, SMOKING, CHD)

# Ajuste del modelo logístico
modelo_chd <- glm(CHD ~ AGE + CHOL + SMOKING, data = data_chd, family = binomial)

# Predicción del valor lineal y la probabilidad
xbeta <- predict(modelo_chd, type = "link")
probabilidades <- predict(modelo_chd, type = "response")

# Gráfica de la sigmoide con clasificación de puntos
x_vals <- seq(min(xbeta) - 1, max(xbeta) + 1, length.out = 500)
y_vals <- 1 / (1 + exp(-x_vals))

# Plot
plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     xlab = expression(X*beta), ylab = "Probabilidad", main = "Curva sigmoide con clasificación")
abline(h = 0.5, lty = 2, col = "red")
abline(v = 0, lty = 2, col = "gray")

# Puntos observados
points(xbeta[data_chd$CHD == 0], data_chd$CHD[data_chd$CHD == 0], col = "darkred", pch = 4)
points(xbeta[data_chd$CHD == 1], data_chd$CHD[data_chd$CHD == 1], col = "darkgreen", pch = 16)

# Leyenda
legend("bottomright", legend = c("Sigmoide", "CHD = 0", "CHD = 1"),
       col = c("blue", "darkred", "darkgreen"), pch = c(NA, 4, 16), lty = c(1, NA, NA), lwd = c(2, NA, NA))



#---- 2. DATOS DE ENFERMEDADES DEL CORAZON ----
##---- 2.1 VARIABLES DEL MODELO ----
edad <- round(runif(n, 35, 80))                      # Edad entre 35 y 80
psistolica <- round(rnorm(n, 130, 20))               # Presión sistólica
colesterol <- round(rnorm(n, 200, 40))               # Colesterol total
diabetes <- rbinom(n, 1, 0.2)                        # Presencia de diabetes (20%)
obesidad <- rbinom(n, 1, 0.3)                        # Obesidad (30%)
tabaco <- rbinom(n, 1, 0.25)                         # Fumador (25%)
##---- 2.2 INICIALIZACION DE PARAMETROS DEL MODELO ----
beta_0 <- -6;      beta_edad <- 0.04; beta_psis <- 0.02; 
beta_col <- 0.01;  beta_diab <- 0.7;  beta_obes <- 0.5; beta_taba <- 0.6;
###---- 2.2.1 MODELO LOGIT----
log_odds <- beta_0 + beta_edad * edad + beta_psis * psistolica + 
  beta_col * colesterol + beta_diab * diabetes + 
  beta_obes * obesidad + beta_taba * tabaco
prob_chd <- 1 / (1 + exp(-log_odds))
CHD <- rbinom(n, 1, prob_chd)
###---- 2.2.2 CREACION DATA FRAME ----
datos_chd <- data.frame(
  edad, psistolica, colesterol, diabetes, obesidad, tabaco, CHD
)
View(datos_chd)
head(datos_chd)
###---- 2.2.3 CREACION DE ARCHIVO .csv ---- 
write.csv(datos_chd, "datos_simulados_CHD.csv", row.names = FALSE)



# Simulación de datos para enfermedad cardíaca (CHD)
set.seed(123)
n <- 200
AGE <- rnorm(n, mean = 55, sd = 10)
CHOL <- rnorm(n, mean = 220, sd = 30)
SMOKING <- rbinom(n, 1, prob = 0.4)

# Simulación del resultado binario CHD
lin_pred <- -6 + 0.05 * AGE + 0.03 * CHOL + 1.5 * SMOKING
prob_CHD <- 1 / (1 + exp(-lin_pred))
CHD <- rbinom(n, 1, prob_CHD)

data_chd <- data.frame(AGE, CHOL, SMOKING, CHD)

# Ajuste del modelo logístico
modelo_chd <- glm(CHD ~ AGE + CHOL + SMOKING, data = data_chd, family = binomial)

# Predicción del valor lineal y la probabilidad
xbeta <- predict(modelo_chd, type = "link")
probabilidades <- predict(modelo_chd, type = "response")

# Gráfica de la sigmoide con clasificación de puntos
x_vals <- seq(min(xbeta) - 1, max(xbeta) + 1, length.out = 500)
y_vals <- 1 / (1 + exp(-x_vals))

# Plot
plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     xlab = expression(X*beta), ylab = "Probabilidad", main = "Curva sigmoide con clasificación")
abline(h = 0.5, lty = 2, col = "red")
abline(v = 0, lty = 2, col = "gray")

# Puntos observados
points(xbeta[data_chd$CHD == 0], data_chd$CHD[data_chd$CHD == 0], col = "darkred", pch = 4)
points(xbeta[data_chd$CHD == 1], data_chd$CHD[data_chd$CHD == 1], col = "darkgreen", pch = 16)

# Leyenda
legend("bottomright", legend = c("Sigmoide", "CHD = 0", "CHD = 1"),
       col = c("blue", "darkred", "darkgreen"), pch = c(NA, 4, 16), lty = c(1, NA, NA), lwd = c(2, NA, NA))


# --------------------------------------------------
# Métricas de evaluación del modelo
# --------------------------------------------------

# Convertir probabilidades a clases usando un umbral de 0.5
pred_clase <- ifelse(probabilidades >= 0.5, 1, 0)

# Matriz de confusión
conf_matrix <- table(Predicho = pred_clase, Real = datos$CHD)
print("Matriz de confusión:")
print(conf_matrix)

# Cálculo de sensibilidad, especificidad, precisión y exactitud
TP <- conf_matrix[2, 2]
TN <- conf_matrix[1, 1]
FP <- conf_matrix[2, 1]
FN <- conf_matrix[1, 2]

sensibilidad <- TP / (TP + FN)
especificidad <- TN / (TN + FP)
precision <- TP / (TP + FP)
exactitud <- (TP + TN) / sum(conf_matrix)

cat("\nSensibilidad:", sensibilidad, 
    "\nEspecificidad:", especificidad, 
    "\nPrecisión:", precision,
    "\nExactitud:", exactitud, "\n")



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



# ============================================
# Script para calcular métricas de clasificación
# ============================================

# Cargar datos simulados (asegúrate de tener el archivo correcto)
datos <- read.csv("datos_simulados_cancer.csv")  # Modifica si usas otro dataset

# Ajustar un modelo de regresión logística
modelo <- glm(Enfermo ~ ., data = datos, family = binomial)

# Obtener probabilidades predichas
probabilidades <- predict(modelo, type = "response")

# Convertir probabilidades en clases binarias usando umbral 0.5
pred <- ifelse(probabilidades > 0.5, 1, 0)

# Real (etiquetas verdaderas)
real <- datos$Enfermo

# Generar matriz de confusión
confusion <- table(Predicho = pred, Real = real)
print("Matriz de Confusión:")
print(confusion)

# Extraer valores
TP <- confusion["1", "1"]
FP <- confusion["1", "0"]
FN <- confusion["0", "1"]
TN <- confusion["0", "0"]

# Calcular métricas
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
specificity <- TN / (TN + FP)

# Imprimir resultados
cat("Recall (Sensibilidad):", recall, "\n")
cat("Precisión:", precision, "\n")
cat("Especificidad:", specificity, "\n")



# Paquetes necesarios
library(pROC)

# Cargar datos simulados
set.seed(123)
n <- 200
edad <- rnorm(n, mean = 50, sd = 10)
grupo <- as.factor(sample(1:4, n, replace = TRUE))
chd <- rbinom(n, 1, prob = 1 / (1 + exp(-(0.05 * edad - 0.5 * as.numeric(grupo)))))

data <- data.frame(edad, grupo, chd)

# Ajustar modelo de regresión logística
modelo <- glm(chd ~ edad + grupo, data = data, family = binomial)

# Predicción de probabilidades
probabilidades <- predict(modelo, type = "response")

# Umbral para clasificación
umbral <- 0.5
predicciones <- ifelse(probabilidades > umbral, 1, 0)

# Matriz de confusión
TP <- sum(predicciones == 1 & data$chd == 1)
FP <- sum(predicciones == 1 & data$chd == 0)
FN <- sum(predicciones == 0 & data$chd == 1)
TN <- sum(predicciones == 0 & data$chd == 0)

# Cálculo de métricas
recall <- TP / (TP + FN)
precision <- TP / (TP + FP)
specificity <- TN / (TN + FP)

cat("Recall:", round(recall, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Specificity:", round(specificity, 3), "\n")

# Calcular y graficar la curva ROC
roc_obj <- roc(data$chd, probabilidades)
plot(roc_obj, col = "blue", main = "Curva ROC")
auc_value <- auc(roc_obj)
cat("AUC:", round(auc_value, 3), "\n")


# ======================================
# VISUALIZACIÓN DE LA CURVA SIGMOIDE
# ======================================

# Suponiendo que ya tienes tu modelo logístico entrenado:
# modelo_chd <- glm(CHD ~ AGE + CHOL + SMOKING, data = data_chd, family = binomial)

# Generamos una secuencia de valores lineales para la combinación Xβ
x_vals <- seq(-10, 10, length.out = 500)
sigmoid <- function(x) 1 / (1 + exp(-x))
y_vals <- sigmoid(x_vals)

# Graficamos la función sigmoide
plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     main = "Función Sigmoide", xlab = expression(X*beta), ylab = "Probabilidad")
abline(h = 0.5, lty = 2, col = "red")
abline(v = 0, lty = 2, col = "gray")

# Leyenda
legend("bottomright", legend = c("Sigmoide", "Umbral = 0.5"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)



# Modelo logístico entrenado (ejemplo con CHD)
modelo_chd <- glm(CHD ~ AGE + CHOL + SMOKING, data = data_chd, family = binomial)

# Calculamos el valor lineal Xβ para cada observación
xbeta <- predict(modelo_chd, type = "link")  # Xβ_i
probabilidades <- predict(modelo_chd, type = "response")  # Sigmoide(Xβ_i)

# Creamos el gráfico base de la función sigmoide
x_vals <- seq(min(xbeta)-1, max(xbeta)+1, length.out = 500)
y_vals <- 1 / (1 + exp(-x_vals))

plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     xlab = expression(X*beta), ylab = "Probabilidad", main = "Curva sigmoide con clasificación")
abline(h = 0.5, lty = 2, col = "red")
abline(v = 0, lty = 2, col = "gray")

# Añadimos los puntos reales clasificados
points(xbeta[data_chd$CHD == 0], data_chd$CHD[data_chd$CHD == 0], col = "darkred", pch = 4)
points(xbeta[data_chd$CHD == 1], data_chd$CHD[data_chd$CHD == 1], col = "darkgreen", pch = 16)

# Leyenda
legend("bottomright", legend = c("Sigmoide", "CHD = 0", "CHD = 1"),
       col = c("blue", "darkred", "darkgreen"), pch = c(NA, 4, 16), lty = c(1, NA, NA), lwd = c(2, NA, NA))


#---- 3. IMPLEMENTACION ----
X <- model.matrix(modelo_cancer); print(X)
y <- datos_cancer$Recidiva; print(y)
beta_hat <- coef(modelo_cancer)
loglik_manual(beta_hat, X, y)  # Debería ser cercano al logLik(modelo)

X <- model.matrix(Recidiva ~ Edad + Tamano_Tumor + 
                    Grado + PR + HER2 + BRCA1 + Ki67 +
                    Hormonoterapia, data = datos_cancer); print(X)
y <- datos_cancer$Recidiva; print(y)
coef_conjugado <- gradiente_conjugado(X, y)
coef_glm <- coef(glm(Recidiva ~ Edad + Tamano_Tumor + Grado + PR + 
                       HER2 + BRCA1 + Ki67 + Hormonoterapia,
                     data = datos_cancer, family = binomial))
round(data.frame(
  Variable = names(coef_glm),
  Coef_GLM = coef_glm,
  Coef_GradConj = coef_conjugado
), 4)
