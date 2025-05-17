#---- 1. SIMULACION DE DATOS DE CANCER ----
##---- 1.1 FIJANDO LA SEMILLA y DIRECTORIO DE TRABAJO ----
setwd("~/Documents/GitHub/DeepLearningReview/0-Notes/DataSets")
##---- 1.2 LLAMADO DE LIBRERIAS ----
library(ggplot2)
library(dplyr)
library(dplyr)
library(ggplot2)
##---- 1.3 INICIALIZANDO LAS VARIABLES Y PARAMETROS ----
n <- 1000
set.seed(123)
##---- 1.4 INICIALIZANDO LAS VARIABLES Y PARAMETROS ----
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
##---- 1.5 GENERACION DEL MODELO  LOGIT ----
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
##---- 1.6 CREACION DATA FRAME ----
datos_cancer <- data.frame(
  Edad = edad,  IMC = bmi,  Menopausia = menopausia,  
  Tamano_Tumor = tamano_tumor,  Grado  = grado_histologico,  
  ER    = er_status,  PR   = pr_status,  HER2  = her2_status,
  BRCA1 = brca1_mut,  Ki67 = ki67,       Quimio = quimio,  
  Hormonoterapia = hormonoterapia,  Recidiva = recidiva)
View(datos_cancer); head(datos_cancer)
##---- 1.7 CREACION DE ARCHIVO .csv ---- 
write.csv(datos_cancer, "datos_simulados_cancer.csv", row.names = FALSE)
##---- 1.8 MODELO DE REGRESIÓN LOGÍSTICA: estimación MLE ----
modelo_cancer <- glm(Recidiva ~ Edad + Tamano_Tumor + IMC + Menopausia +
                       Grado + PR + ER + HER2 + BRCA1 + Ki67 + Quimio + Hormonoterapia,
              data = datos_cancer, family = binomial)
summary(modelo_cancer)
##---- 1.9 RAZONES DE MOMIOS E INTERVALOS DE CONFIANZA ----
exp_coef <- exp(coef(modelo_cancer))
exp_ci <- exp(confint(modelo_cancer))
##---- 1.10 RESUMEN DEL CALCULO DE MOMIOS ----
or_tabla <- data.frame(
  Variable = names(exp_coef),
  OR = round(exp_coef, 3),
  IC_inf = round(exp_ci[,1], 3),
  IC_sup = round(exp_ci[,2], 3)
)
print(or_tabla)
##---- 1.11 PREPARANDO LA GRAFICA ---
or_tabla_plot <- or_tabla[or_tabla$Variable != "(Intercept)", ]
or_tabla_plot$Variable <- factor(or_tabla_plot$Variable, 
                                 levels = rev(or_tabla_plot$Variable))
##---- 1.12 GENERACION DE LA GRAFICA ----
ggplot(or_tabla_plot, aes(x = OR, y = Variable)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Forest Plot del Modelo de Estudio de Cancer",
       x = "Odds Ratio (OR)", y = NULL) +
  theme_minimal()
##---- 1.13 GRAFICANDO PREDICCIONES ----
datos_cancer$prob_predicha <- predict(modelo_cancer, type = "response")
datos_cancer$clasificacion <- ifelse(datos_cancer$prob_predicha >= 0.5, 1, 0)
table(Real = datos_cancer$Recidiva, Predicho = datos_cancer$clasificacion)
ggplot(datos_cancer, aes(x = Tamano_Tumor, y = prob_predicha)) +
  geom_point(aes(color = as.factor(Recidiva)), alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = binomial), se = FALSE, color = "black") +
  labs(title = "Curva sigmoide estimada",
       subtitle = "Variable: Tamaño del tumor",
       x = "Tamaño del tumor (mm)",
       y = "Probabilidad estimada de recidiva",
       color = "Recidiva real") + theme_minimal()
#---- 2 ECUACIONES IMPORTANTES ----

##---- 2.1 CALCULO DE LA LOG-VEROSIMILITUD ----  
loglik_manual <- function(beta, X, y) {
  eta <- X %*% beta;
  p <- 1 / (1 + exp(-eta));
  sum(y * log(p) + (1 - y) * log(1 - p));
  }
##---- 2.2 Función sigmoide ----
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}
##---- 2.3 Función de pérdida: log-verosimilitud negativa ----
log_likelihood <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  -sum(y * log(p + 1e-10) + (1 - y) * log(1 - p + 1e-10))  # pequeña corrección numérica
}
##---- 2.4 Gradiente de la pérdida ----
log_likelihood_grad <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  t(X) %*% (p - y)
}

# Hessiana de la log-verosimilitud negativa
hessian <- function(beta, X) {
  p <- sigmoid(X %*% beta)
  W <- diag(as.numeric(p * (1 - p)))
  t(X) %*% W %*% X
}

newton_raphson_logit <- function(X, y, tol = 1e-6, max_iter = 100) {
  beta <- rep(0, ncol(X))  # Paso 1: inicialización
  for (i in 1:max_iter) {
    grad <- gradient(beta, X, y)        # Paso 2: gradiente
    H <- hessian(beta, X)               # Paso 2: hessiana
    beta_new <- beta - solve(H, grad)   # Paso 3: actualización
    
    # Paso 4: criterio de convergencia
    if (max(abs(beta_new - beta)) < tol) {
      cat("Convergió en", i, "iteraciones\n")
      return(beta_new)
    }
    
    beta <- beta_new
  }
  warning("No convergió en el número máximo de iteraciones")
  return(beta)
}

# Gradiente: g(β)
grad_loglik <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  t(X) %*% (p - y)
}

# Hessiana: H(β)
hess_loglik <- function(beta, X) {
  p <- sigmoid(X %*% beta)
  W <- diag(as.numeric(p * (1 - p)))
  t(X) %*% W %*% X
}

newton_raphson_beta_k <- function(X, y, tol = 1e-6, max_iter = 100) {
  beta_k <- rep(0, ncol(X))  # Paso 1: inicialización
  
  for (k in 1:max_iter) {
    g_k <- grad_loglik(beta_k, X, y)   # Paso 2: gradiente
    H_k <- hess_loglik(beta_k, X)      # Paso 2: hessiana
    
    # Paso 3: actualización
    beta_k1 <- beta_k - solve(H_k, g_k)
    
    # Paso 4: criterio de convergencia
    if (max(abs(beta_k1 - beta_k)) < tol) {
      cat("Convergió en", k, "iteraciones\n")
      return(beta_k1)
    }
    
    beta_k <- beta_k1
  }
  
  warning("No convergió en el número máximo de iteraciones")
  return(beta_k)
}

##---- Gradiente Conjugado no lineal (Fletcher-Reeves simplificado) ----
gradiente_conjugado <- function(X, y, max_iter = 1000, tol = 1e-6) {
  n <- ncol(X)
  beta <- rep(0, n)
  g <- log_likelihood_grad(beta, X, y)
  d <- -g
  for (i in 1:max_iter) {
    alpha <- 0.01
    beta_new <- beta + alpha * d
    g_new <- log_likelihood_grad(beta_new, X, y)
    if (sqrt(sum(g_new^2)) < tol) {
      cat("Convergencia alcanzada en iteración", i, "\n")
      return(beta_new)
    }
    beta_FR <- sum(g_new^2) / sum(g^2)
    d <- -g_new + beta_FR * d
    beta <- beta_new
    g <- g_new
  }
  warning("No convergió") # investigar como se implemento 
  return(beta) # el Gradiente conjugado
}


