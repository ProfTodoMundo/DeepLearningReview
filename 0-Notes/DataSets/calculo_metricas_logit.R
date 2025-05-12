
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
