#---- 1. CARGA DE LIBRERÍAS ----
library(ggplot2)
library(dplyr)
library(pROC)
library(tidyr)
#---- 2. FUNCIONES AUXILIARES ----
##---- 2.1 FUNCION SIGMOIDE ----
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}
##---- 2.2 FUNCION LOG-VEROSIMILITUD ----
log_likelihood <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  -sum(y * log(p + 1e-10) + (1 - y) * log(1 - p + 1e-10))
}
##---- 2.3 FUNCION GRADIENTE ----
log_likelihood_grad <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  t(X) %*% (p - y)
}
##---- 2.4 CALCULO DE LA MATRIZ HESSIANA ----
hessian <- function(beta, X) {
  p <- sigmoid(X %*% beta)
  W <- diag(as.numeric(p * (1 - p)))
  t(X) %*% W %*% X
}
##---- 2.5 ALGORITMO DE NEWTON RAPHSON ----
newton_raphson <- function(X, y, tol = 1e-6, max_iter = 100) {
  beta <- rep(0, ncol(X))
  normas <- numeric()
  for (i in 1:max_iter) {
    grad <- log_likelihood_grad(beta, X, y)
    H <- hessian(beta, X)
    beta_new <- beta - solve(H, grad)
    normas[i] <- sqrt(sum(grad^2))
    if (max(abs(beta_new - beta)) < tol) {
      cat("NR convergió en", i, "iteraciones\n")
      return(list(beta = beta_new, iter = i, normas = normas))
    }
    beta <- beta_new
  }
  warning("NR no convergió")
  return(list(beta = beta, iter = max_iter, normas = normas))
}
##---- 2.6 METODO DEL GRADIENTE CONJUGADO ----
gradiente_conjugado <- function(X, y, max_iter = 1000, tol = 1e-6) {
  n <- ncol(X)
  beta <- rep(0, n)
  g <- log_likelihood_grad(beta, X, y)
  d <- -g
  normas <- numeric()
  for (i in 1:max_iter) {
    alpha <- 0.01
    beta_new <- beta + alpha * d
    g_new <- log_likelihood_grad(beta_new, X, y)
    normas[i] <- sqrt(sum(g_new^2))
    if (normas[i] < tol) {
      cat("GC convergió en", i, "iteraciones\n")
      return(list(beta = beta_new, iter = i, normas = normas))
    }
    beta_FR <- sum(g_new^2) / sum(g^2)
    d <- -g_new + beta_FR * d
    beta <- beta_new
    g <- g_new
  }
  warning("GC no convergió")
  return(list(beta = beta, iter = max_iter, normas = normas))
}
##---- 3 INICIALIZANDO LAS VARIABLES Y PARAMETROS ----
n <- 1000
set.seed(123)
##---- 3.1 INICIALIZANDO LAS VARIABLES Y PARAMETROS ----
edad <- round(rnorm(n, mean = 55, sd = 10))                        # Edad en años
bmi  <- round(rnorm(n, mean = 27, sd = 4), 1)                      # Índice de Masa Corporal
menopausia   <- sample(c("Pre", "Post"), n, replace = TRUE, prob = c(0.4, 0.6)) # Estado menopáusico
menopausia_post <- ifelse(menopausia == "Post", 1, 0)
tamano_tumor <- round(rnorm(n, mean = 25, sd = 10), 1)                # Tamaño del tumor (mm)
er_status    <- sample(c(0,1), n, replace = TRUE, prob = c(0.3, 0.7)) # Receptor de estrógeno
pr_status    <- sample(c(0,1), n, replace = TRUE, prob = c(0.4, 0.6)) # Receptor de progesterona
her2_status  <- sample(c(0,1), n, replace = TRUE, prob = c(0.8, 0.2)) # HER2
brca1_mut    <- sample(c(0,1), n, replace = TRUE, prob = c(0.95, 0.05)) # Mutación BRCA1
ki67         <- round(runif(n, min = 5, max = 60), 1)                   # Índice Ki-67 (%)
quimio       <- rbinom(n, 1, prob = 0.5)                                # Quimioterapia
hormonoterapia    <- rbinom(n, 1, prob = 0.6)                           # Hormonoterapia
grado_histologico <- sample(1:3, n, replace = TRUE, prob = c(0.3, 0.4, 0.3))  # Grado histológico
##---- 3.2 GENERACION DEL MODELO  LOGIT ----
lp <- -3.5 +
  0.04 * (edad - 50) + 0.02 * (bmi - 25) +  # efecto leve del IMC
  0.5  * menopausia_post +       # efecto positivo para postmenopáusicas
  0.03 * (tamano_tumor - 20) +
  0.3  * er_status -             # efecto protector por receptor de estrógeno
  0.4  * pr_status +  0.8  * her2_status +
  1.2  * brca1_mut -  0.05 * ki67 +
  0.6  * quimio +                  # quimioterapia reduce riesgo de recidiva
  0.5  * hormonoterapia -  0.1 * (grado_histologico - 1)
prob_recidiva <- 1 / (1 + exp(-lp))
recidiva      <- rbinom(n, 1, prob = prob_recidiva)
datos_cancer <- data.frame(
  Edad = edad,  IMC = bmi,  Menopausia = menopausia,  
  Tamano_Tumor = tamano_tumor,  Grado  = grado_histologico,  
  ER    = er_status,  PR   = pr_status,  HER2  = her2_status,
  BRCA1 = brca1_mut,  Ki67 = ki67,       Quimio = quimio,  
  Hormonoterapia = hormonoterapia,  Recidiva = recidiva)
View(datos_cancer)
#---- 4. MODELOS Y ESTIMACIONES ----
X <- model.matrix(recidiva ~ ., data = datos_cancer)
y <- datos_cancer$Recidiva
##---- 4.1 APLICACION DE LA FUNCION GENERALIZED LINEAR MODELS ---- 
modelo_glm <- glm(Recidiva ~ ., data = datos_cancer, family = binomial)
coef_glm <- coef(modelo_glm)
##---- 4.2 APLICACION DE NEWTON Y GRADIENTE ----
res_nr <- newton_raphson(X, y)
res_gc <- gradiente_conjugado(X, y)
#---- 5. TABLA DE COEFICIENTES ----
tabla_comparacion <- data.frame(
  Variable = names(coef_glm),
  Coef_GLM = round(coef_glm, 4),
  Coef_NR = round(res_nr$beta, 4),
  Coef_GC = round(res_gc$beta, 4)
)
print(tabla_comparacion)
#---- 6. MATRICES DE CONFUSIÓN ----
y_pred_glm <- ifelse(predict(modelo_glm, type = "response") > 0.5, 1, 0)
y_pred_nr <- ifelse(sigmoid(X %*% res_nr$beta) > 0.5, 1, 0)
y_pred_gc <- ifelse(sigmoid(X %*% res_gc$beta) > 0.5, 1, 0)
cat("\nMatriz de confusión (GLM):\n"); print(table(Real = y, Predicho = y_pred_glm))
cat("\nMatriz de confusión (NR):\n"); print(table(Real = y, Predicho = y_pred_nr))
cat("\nMatriz de confusión (GC):\n"); print(table(Real = y, Predicho = y_pred_gc))
#---- 7. CURVA DE CONVERGENCIA ----
df_convergencia <- data.frame(
  Iteracion = 1:max(length(res_nr$normas), length(res_gc$normas)),
  Newton_Raphson = c(res_nr$normas, rep(NA, max(0, length(res_gc$normas) - length(res_nr$normas)))),
  Gradiente_Conjugado = c(res_gc$normas, rep(NA, max(0, length(res_nr$normas) - length(res_gc$normas))))
)
df_convergencia_long <- pivot_longer(df_convergencia, cols = -Iteracion, names_to = "Metodo", values_to = "Norma")
ggplot(df_convergencia_long, aes(x = Iteracion, y = Norma, color = Metodo)) +
  geom_line(size = 1.1) +
  scale_y_log10() +
  labs(title = "Curva de convergencia de los métodos",
       y = "Norma del gradiente (log)", x = "Iteración",
       color = "Método") +
  theme_minimal()
#---- 8. CURVAS ROC Y AUC ----
prob_glm <- predict(modelo_glm, type = "response")
prob_nr  <- sigmoid(X %*% res_nr$beta)
prob_gc  <- sigmoid(X %*% res_gc$beta)

roc_glm <- roc(y, prob_glm)
roc_nr  <- roc(y, prob_nr)
roc_gc  <- roc(y, prob_gc)

auc_glm <- auc(roc_glm)
auc_nr  <- auc(roc_nr)
auc_gc  <- auc(roc_gc)

cat("\nAUC (GLM):", round(auc_glm, 4))
cat("\nAUC (NR):", round(auc_nr, 4))
cat("\nAUC (GC):", round(auc_gc, 4))
#---- 9. GRAFICA ----
plot(roc_glm, col = "blue", lwd = 2, main = "Curvas ROC comparadas")
lines(roc_nr, col = "darkgreen", lwd = 2, lty = 2)
lines(roc_gc, col = "red", lwd = 2, lty = 3)
legend("bottomright",
       legend = c(
         paste("GLM (AUC =", round(auc_glm, 3), ")"),
         paste("Newton-Raphson (AUC =", round(auc_nr, 3), ")"),
         paste("Grad. Conjugado (AUC =", round(auc_gc, 3), ")")
       ),
       col = c("blue", "darkgreen", "red"),
       lty = 1:3, lwd = 2, cex = 0.8)


