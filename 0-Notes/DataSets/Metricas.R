

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


##---- Aplicación a los datos simulados de cáncer ----
datos_cancer$edad
datos_cancer$tamano_tumor
datos_cancer$sobrevivio
X_cancer <- as.matrix(cbind(1, datos_cancer$edad, datos_cancer$tamano_tumor))
y_cancer <- as.matrix(datos_cancer$sobrevivio)
cg_cancer <- conjugate_gradient(X_cancer, y_cancer)
print("Coeficientes estimados (cáncer):")
print(cg_cancer$beta)


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

