#---- 0. SEMILLA INICIAL Y LIBRERIAS---- 
set.seed(123)
##---- Librerias ----
library(pROC)
library(ggplot2)
library(broom)
library(dplyr)
# ---- 1. SIMULACION DE DATOS DE CANCER ----
#---- 2. EJEMPLO UNIFICADO ----
n <- 1000
#---- 2.1 VARIABLES A CONSIDERAR ----
###---- 2.1.1 EDAD ----
edad_cancer  <- round(rnorm(n, mean = 55, sd = 10))
###---- 2.1.2 TAMANHO DEL TUMOR ----
tamano_tumor <- round(rnorm(n, mean = 2.5, sd = 1), 1)
###---- 2.1.3 PROGESTERONA ----
progesterona <- round(rnorm(n, mean = 15, sd = 5), 1)
###---- 2.1.4 HER ----
HER2 <- rbinom(n, 1, 0.3)
###---- 2.1.5 RE ----
RE  <- rbinom(n, 1, 0.6)
###----2.1.6  RP ----
RP  <- rbinom(n, 1, 0.5)
###---- 2.1.7 DENSIDAD MAMARIA ----
densidad_mamaria  <- rnorm(n, mean = 0.6, sd = 0.2)
###----2.1.8 MARCADOR A ----
marker_A <- round(rnorm(n, 1.0, 0.3), 1)
###---- 2.1.9 MARCADOR B ----
marker_B <- round(rnorm(n, 1.2, 0.4), 1)
##---- 2.2 LOG ODDS ----
log_odds_cancer <- -4 + 1.348 * marker_A + 0.898 * marker_B +
  0.03 * edad_cancer + 0.05 * tamano_tumor -  0.1 * progesterona +
  0.8 * HER2 + 0.6 * RE + 0.5 * RP +  0.3 * densidad_mamaria
##---- 2.3 PROBABILIDAD SOBREVIVIR ----
probabilidad_cancer <- 1 / (1 + exp(-log_odds_cancer))
##---- SOBREVIVENCIA ----
recidiva <- rbinom(n, 1, probabilidad_cancer)
##---- 2.4 GENERACION DEL DATAFRAME ----
datos_cancer <- data.frame(
  edad = edad_cancer,  tamano_tumor = tamano_tumor,
  progesterona = progesterona,HER2 = HER2, RE = RE,
  RP = RP, Densidad = densidad_mamaria,
  MarcadorA = marker_A,MarcadorB = marker_B,
  sobrevivio = recidiva)
##---- 2.5 GENERACION DEL MODELO ----
modelo_cancer <- glm(sobrevivio ~  .,data = datos_cancer, family = binomial)
summary(modelo_cancer)
miscoeficientes <- coef(modelo_cancer)
##---- 2.6 COMPARACION DE COEFICIENTES ----
coef_reales <- c(
  `(Intercept)` = -4,
  edad = 0.03,
  tamano_tumor = 0.05,
  progesterona = -0.1,
  HER2 = 0.8,
  RE = 0.6,
  RP = 0.5,
  Densidad = 0.3,
  MarcadorA = 1.348,
  MarcadorB = 0.898
)
comparativo <- data.frame(
  Coef_estimados = round(miscoeficientes,3),
  Coef_reales = round(coef_reales[names(miscoeficientes)], 3)
)
comparativo$diferencia <- round(comparativo$Coef_estimados-comparativo$Coef_reales,3)
print(comparativo)
##---- 2.7 PREDICCION ----
datos_cancer$prediccion <- predict(modelo_cancer,type = "response")
datos_cancer$clasificacion <- ifelse(datos_cancer$prediccion >= 0.5, 1, 0)
##---- 2.8 GRAFICANDO ----
library(ggplot2)
###---- 2.8.1 Tamaño del tumor ----
ggplot(datos_cancer, aes(x = tamano_tumor)) +
  geom_point(aes(y = prediccion, color = factor(sobrevivio)), alpha = 0.5) +    
  stat_smooth(aes(y = sobrevivio), 
              method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Recidiva vs Tamaño del Tumor",
       x = "Tamaño del tumor (cm)",
       y = "Probabilidad estimada",
       color = "Recidiva (1 = sí)") +
  theme_minimal()
###---- 2.8.2 Edad ----
ggplot(datos_cancer, aes(x = edad)) +
  geom_point(aes(y = prediccion, color = factor(sobrevivio)), alpha = 0.5) +
  stat_smooth(aes(y = sobrevivio),
              method = "glm",
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Recidiva vs Edad",
       x = "Edad (años)",
       y = "Probabilidad estimada",
       color = "Recidiva (1 = sí)") + 
  theme_minimal()
###---- 2.8.3 Progesterona ----
ggplot(datos_cancer, aes(x = progesterona)) +
  geom_point(aes(y=prediccion, color = factor(sobrevivio)), alpha = 0.5) +     
  stat_smooth(aes(y = sobrevivio),
              method = "glm",
              method.args = list(family = "binomial"), 
              se = FALSE, color="black") +  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Recidiva vs Progesterona",
       y = "Probabilidad estimada", color = "Recidiva (1 = sí)")+ 
  theme_minimal()
###---- 2.8.4 Densidad Mamaria ----
ggplot(datos_cancer, aes(x = Densidad)) +
  geom_point(aes(y=prediccion,color = factor(sobrevivio)), alpha = 0.5) +     
  stat_smooth(aes(y = sobrevivio),
              method = "glm",
              method.args = list(family = "binomial"), 
              se = FALSE, color="black") +  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Recidiva vs Densidad mamaria",
       y = "Probabilidad estimada", color = "Recidiva (1 = sí)")+ 
  theme_minimal()
###---- 2.8.5 Marcador A: Biomarcador A ----
ggplot(datos_cancer, aes(x = MarcadorA)) +
  geom_point(aes(y=prediccion,color = factor(sobrevivio)), alpha = 0.5) +     
  stat_smooth(aes(y = sobrevivio),
              method = "glm",
              method.args = list(family = "binomial"), 
              se = FALSE, color="black") +  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Recidiva vs MarcadorA",
       y = "Probabilidad estimada", color = "Recidiva (1 = sí)")+ 
  theme_minimal()
###---- 2.8.6 Marcador B: Biomarcador B ----
ggplot(datos_cancer, aes(x = MarcadorB)) +
  geom_point(aes(y=prediccion,color = factor(sobrevivio)), alpha = 0.5) +     
  stat_smooth(aes(y = sobrevivio),
              method = "glm",
              method.args = list(family = "binomial"), 
              se = FALSE, color="black") +  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Recidiva vs MarcadorB",
       y = "Probabilidad estimada", color = "Recidiva (1 = sí)")+ 
  theme_minimal()
###---- 2.8.7 RP: Receptor de Progesterona ----
ggplot(datos_cancer, aes(x = RP)) +
  geom_point(aes(y = prediccion, color = factor(sobrevivio)), alpha = 0.5) +     
  stat_smooth(aes(y = sobrevivio),
              method = "glm",
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Recidiva vs RP",
       x = "RP",
       y = "Probabilidad estimada",
       color = "Recidiva (1 = sí)") + 
  theme_minimal()
###---- 2.8.8 RE: Receptor de Estrogeno ----
ggplot(datos_cancer, aes(x = RE)) +
  geom_point(aes(y = prediccion, color = factor(sobrevivio)), alpha = 0.5) +     
  stat_smooth(aes(y = sobrevivio),
              method = "glm",
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Recidiva vs RE",
       x = "RE",
       y = "Probabilidad estimada",
       color = "Recidiva (1 = sí)") + 
  theme_minimal()
###---- 2.8.9 HER2: Receptor del factor de crecimiento epidermico humano ----
ggplot(datos_cancer, aes(x = HER2)) +
  geom_point(aes(y = prediccion, color = factor(sobrevivio)), alpha = 0.5) +     
  stat_smooth(aes(y = sobrevivio),
              method = "glm",
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Recidiva vs HER2",
       x = "HER2",
       y = "Probabilidad estimada",
       color = "Recidiva (1 = sí)") + 
  theme_minimal()
##---- 2.9 MATRIZ DE CONFUSION ---- 
library(caret)
matriz1 <- table(Real = datos_cancer$sobrevivio, Prediccion = datos_cancer$clasificacion)
print(matriz1)
TN <- matriz1["0","0"]
TP <- matriz1["1","1"]
FP <- matriz1["0","1"]
FN <- matriz1["1","0"]

Accuracy    <- (TP+TN)/sum(matriz1)
Precision   <- TP/(TP+FP)
Rrecall     <- TP/(TP+FN)
Specificity <- TN/(TN+FP)
f1_score    <- 2*(Precision*Rrecall)/(Precision+Rrecall)

Resultados <- data.frame(
  Accuracy = round(Accuracy,3),
  Precision = round(Precision,3),
  Recall = round(Rrecall,3),
  Specificity = round(Specificity,3),
  F1_Score = round(f1_score,3)
)
print(Resultados)
library(pROC)
roc_cancer <- roc(datos_cancer$sobrevivio, datos_cancer$prediccion)
auc(roc_cancer)
plot(roc_cancer, col = "blue", lwd = 2, main = "Curva ROC - Modelo cáncer simulado")
abline(a = 0, b = 1, col = "gray", lty = 2)  # Línea diagonal = azar
#---- 2.10 Training/Test Set ----
set.seed(123)
n <- nrow(datos_cancer)
indices_entrenamiento <- sample(1:n, size = 0.7 * n)
train_data <- datos_cancer[indices_entrenamiento, ]
test_data  <- datos_cancer[-indices_entrenamiento, ]
###---- 2.10.1 Ajuste del modelo con training ----
modelo_train <- glm(sobrevivio ~  .,
                    data = train_data, 
                    family = binomial)
summary(modelo_train)
###---- 2.10.2 Prediccion con test set ----
test_data$prob <- predict(modelo_train, 
                          newdata = test_data, 
                          type = "response")
test_data$clasificacion <- ifelse(test_data$prob >= 0.5, 1, 0)
###---- 2.10.3 Matriz de confusion y metricas ----
matriz_test <- table(
  factor(test_data$sobrevivio, levels = c(0, 1)),
  factor(test_data$clasificacion, levels = c(0, 1))
)

TP <- matriz_test["1", "1"]
TN <- matriz_test["0", "0"]
FP <- matriz_test["0", "1"]
FN <- matriz_test["1", "0"]

Accuracy    <- (TP + TN) / sum(matriz_test)
Precision   <- TP / (TP + FP)
rRecall     <- TP / (TP + FN)
Specificity <- TN / (TN + FP)
F1_Score    <- 2 * (Precision * rRecall) / (Precision + rRecall)

Resultados_test <- data.frame(
  Accuracy = round(Accuracy, 3),
  Precision = round(Precision, 3),
  Recall = round(rRecall, 3),
  Specificity = round(Specificity, 3),
  F1_Score = round(F1_Score, 3)
)
print(Resultados_test)
###---- 2.10.4 Curva ROC test ----
library(pROC)
roc_test <- roc(test_data$sobrevivio, test_data$prob)
plot(roc_test, col = "blue", lwd = 2,
     main = "Curva ROC - Test set (Cancer)")
abline(a = 0, b = 1, col = "gray", lty = 2)
auc(roc_test)
#---- 2.11 FUNCIONES MANUALES DE OPTIMIZACION ----
##---- 2.11.1 Funcion Sigmoide ----
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}
##---- 2.11.2 Log-verosimilitud negativa ----
log_likelihood <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  -sum(y * log(p + 1e-10) + (1 - y) * log(1 - p + 1e-10))
}
##---- 2.11.3 Gradiente de la log-verosimilitud ----
log_likelihood_grad <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  t(X) %*% (p - y)
}
gradiente_logL <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  t(X) %*% (y - p)
}
##---- 2.11.4 Hessiana de la log-verosimilitud ----
hessian <- function(beta, X) {
  p <- sigmoid(X %*% beta)
  W <- diag(as.numeric(p * (1 - p)))
  t(X) %*% W %*% X
}
hessiana_logL <- function(beta, X) {
  p <- sigmoid(X %*% beta)
  W <- diag(as.numeric(p * (1 - p)))
  -t(X) %*% W %*% X
}
##---- 2.11.5 Newton-Raphson para regresion 1 ----
newton_raphson_1 <- function(X, y, tol = 1e-6, max_iter = 5000) {
  beta <- rep(0, ncol(X))  # Paso 1
  for (t in 1:max_iter) {
    grad <- gradiente_logL(beta, X, y)      # Paso 2
    H    <- hessiana_logL(beta, X)             # Paso 2
    beta_new <- beta - solve(H) %*% grad    # Paso 3
    # Paso 4: convergencia
    if (max(abs(beta_new - beta)) < tol) {
      cat("✅ Algoritmo 1 convergió en", t, "iteraciones\n")
      return(beta_new)
    }
    beta <- beta_new
  }
  warning("⚠️ Algoritmo 1 no convergió")
  return(beta)
}
##---- 2.11.5 Newton-Raphson para regresion 2 ----
newton_raphson_2 <- function(X, y, tol = 1e-6, max_iter = 100) {
  beta <- rep(0, ncol(X))  # β⁽⁰⁾
  for (k in 1:max_iter) {
    gk <- gradiente_logL(beta, X, y)     # g(β⁽ᵏ⁾)
    Hk <- hessiana_logL(beta, X)         # H(β⁽ᵏ⁾)
    
    beta_next <- beta - solve(Hk) %*% gk
    
    if (max(abs(beta_next - beta)) < tol) {
      cat("✅ Algoritmo 2 convergió en", k, "iteraciones\n")
      return(beta_next)
    }
    beta <- beta_next
  }
  warning("⚠️ Algoritmo 2 no convergió")
  return(beta)
}
##---- 2.12 PRUEBA DE FUNCIONES MANUALES CON DATOS ----
X <- model.matrix(sobrevivio ~ ., data = datos_cancer)
y <- datos_cancer$sobrevivio
####---- 2.12.1 Aplicación ----
beta1 <- newton_raphson_1(X, y)
beta2 <- newton_raphson_2(X, y)
beta_glm <- coef(glm(sobrevivio ~ ., data = datos_cancer, family = binomial))
####---- 2.12.2 Comparación ----
coef_names <- names(beta_glm)
####---- 2.12.3 Crear tabla comparativa ----
tabla_coef <- data.frame(
  Variable = coef_names,
  GLM = round(beta_glm, 4),
  Newton_Raphson_1 = round(beta1, 4),
  Newton_Raphson_2 = round(beta2, 4)
)
print(tabla_coef)
####---- 2.12.4 Convertir tabla a formato largo para ggplot2 ----
library(tidyr)
library(dplyr)
tabla_larga <- tabla_coef %>%
  pivot_longer(cols = -Variable, names_to = "Metodo", values_to = "Coeficiente")
####---- 2.12.5 Graficar comparativa ----
library(ggplot2)
ggplot(tabla_larga, aes(x = Variable, y = Coeficiente, color = Metodo)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  labs(title = "Comparación de coeficientes estimados",
       y = "Valor del coeficiente",
       x = "Variable explicativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


comparativo_df <- data.frame(
  Variable = names(beta_glm),
  GLM = round(beta_glm, 4),
  Newton_Raphson = round(beta1, 4),
  Grad_Conjugado = round(beta2, 4)
)
#---- FIN ----



