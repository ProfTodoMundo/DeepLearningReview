
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
