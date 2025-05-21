#---- SEMILLA INICIAL Y LIBRERIAS---- 
set.seed(123)
##---- Librerias ----
library(pROC)
library(ggplot2)
library(broom)
#---- 1. VARIABLES A CONSIDERAR ----
n <- 1000
##---- 1.1 Edad ----
edad <- round(rnorm(n, mean = 55, sd = 10))
##---- 1.2 Colesterol ----
colesterol <- round(rnorm(n, mean = 220, sd = 30), 1)
##---- 1.3 Presion Arterial (sistolica) ----
presion <- round(rnorm(n, mean = 135, sd = 20), 1)
##---- 1.4 Fumador ----
tabaco <- rbinom(n, 1, 0.25)                        
##---- 1.5 Presencia de Diabetes ----
diabetes <- rbinom(n, 1, 0.2)                     
##---- 1.6 Presencia de Obsesidad ----
obesidad <- rbinom(n, 1, 0.3) 
#---- 2 LOG ODDS ----
log_odds_chd <- -8 +
  0.035 * edad +        # impacto moderado por año
  0.015 * colesterol +  # efecto más suave
  0.02 * presion +      # efecto leve (presión ya alta)
  1.0  * tabaco +       # riesgo fuerte si fuma
  1.5  * diabetes +     # riesgo fuerte si es diabético
  0.8  * obesidad       # riesgo moderado-alto si tiene obesidad
#---- 3. PROBABILIDAD SOBREVIVIR ----
probabilidad_chd <- 1 / (1 + exp(-log_odds_chd))
#---- 4. SOBREVIVENCIA ----
enfermedad_corazon <- rbinom(n, 1, probabilidad_chd)
#---- 5. GENERACION DEL DATAFRAME ----
datos_chd <- data.frame(
  Edad = edad, 
  Colesterol = colesterol, 
  Presion = presion, 
  Tabaquismo = tabaco,
  Diabetes = diabetes,
  Obesidad = obesidad,
  Enfermedad = enfermedad_corazon)
#---- 6. GENERACION DEL MODELO ----
modelo_chd <- glm(Enfermedad ~ Edad + Colesterol + 
                    Presion + Tabaquismo + Diabetes +
                    Obesidad, 
                    data = datos_chd, family = binomial)
summary(modelo_chd)
miscoeficientes <- coef(modelo_chd)
#---- 7 COMPARACION DE COEFICIENTES ----
coef_reales <- c(
  `(Intercept)` = -6,
  Edad = 0.04,
  Colesterol = 0.03,
  Presion = 0.05,
  Tabaquismo = 0.8,
  Diabetes = 1.2,
  Obesidad = 0.5
)
comparativo <- data.frame(
  Coef_estimados = round(miscoeficientes,3),
  Coef_reales = round(coef_reales[names(miscoeficientes)], 3)
)
comparativo$diferencia <- round(comparativo$Coef_estimados-comparativo$Coef_reales,3)
print(comparativo)
#---- 8. PREDICCION ----
datos_chd$prediccion <- predict(modelo_chd,type = "response")
datos_chd$clasificacion <- ifelse(datos_chd$prediccion >= 0.5, 1, 0)
##---- 8.1 MATRIZ DE CONFUSION ---- 
library(caret)
matriz1 <- table(Real = datos_chd$Enfermedad, 
                 Prediccion = datos_chd$clasificacion)
print(matriz1)
TN <- matriz1["0","0"]
TP <- matriz1["1","1"]
FP <- matriz1["0","1"]
FN <- matriz1["1","0"]

Accuracy    <- (TP+TN)/sum(matriz1)
Precision   <- TP/(TP+FP)
rRecall     <- TP/(TP+FN)
Specificity <- TN/(TN+FP)
f1_score    <- 2*(Precision*rRecall)/(Precision+rRecall)

Resultados <- data.frame(
  Accuracy = round(Accuracy,3),
  Precision = round(Precision,3),
  Recall = round(rRecall,3),
  Specificity = round(Specificity,3),
  F1_Score = round(f1_score,3)
)
print(Resultados)
library(pROC)
roc_chd <- roc(datos_chd$Enfermedad, datos_chd$prediccion)
auc(roc_chd)
plot(roc_chd, col = "blue", lwd = 2, 
     main = "Curva ROC - Modelo Enfermedad Coronaria (Simulacion)")
abline(a = 0, b = 1, col = "gray", lty = 2)  # Línea diagonal = azar
#---- 9. GRAFICANDO ----
library(ggplot2)
##---- 9.1 Edad ----
ggplot(datos_chd, aes(x = Edad)) +
  geom_point(aes(y = prediccion, color = factor(Enfermedad)), alpha = 0.5) +
  stat_smooth(aes(y = Enfermedad), method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Edad",
       x = "Edad (años)", y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()
#---- 9.2 Colesterol ----
ggplot(datos_chd, aes(x = Colesterol)) +
  geom_point(aes(y = prediccion, color = factor(Enfermedad)), alpha = 0.5) +
  stat_smooth(aes(y = Enfermedad), method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Colesterol",
       x = "Colesterol (mg/dL)", y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()
#---- 9.3 Presión Arterial ----
ggplot(datos_chd, aes(x = Presion)) +
  geom_point(aes(y = prediccion, color = factor(Enfermedad)), alpha = 0.5) +
  stat_smooth(aes(y = Enfermedad), method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Presión Arterial",
       x = "Presión Sistólica (mmHg)", y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()
#---- 9.4 Tabaquismo ----
ggplot(datos_chd, aes(x = Tabaquismo)) +
  geom_point(aes(y = prediccion, color = factor(Enfermedad)), alpha = 0.5) +
  stat_smooth(aes(y = Enfermedad), method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Tabaquismo",
       x = "Tabaquismo (0 = No, 1 = Sí)", y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()
#---- 9.5 Diabetes ----
ggplot(datos_chd, aes(x = Diabetes)) +
  geom_point(aes(y = prediccion, color = factor(Enfermedad)), alpha = 0.5) +
  stat_smooth(aes(y = Enfermedad), method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Diabetes",
       x = "Diabetes (0 = No, 1 = Sí)", y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()
#---- 9.6 Obesidad ----
ggplot(datos_chd, aes(x = Obesidad)) +
  geom_point(aes(y = prediccion, color = factor(Enfermedad)), alpha = 0.5) +
  stat_smooth(aes(y = Enfermedad), method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Probabilidad de Enfermedad Coronaria vs Obesidad",
       x = "Obesidad (0 = No, 1 = Sí)", y = "Probabilidad estimada", color = "Enfermedad") +
  theme_minimal()
#---- 10. Training/Test Set ----
set.seed(123)
n <- nrow(datos_chd)
indices_entrenamiento <- sample(1:n, size = 0.7 * n)
train_data <- datos_chd[indices_entrenamiento, ]
test_data  <- datos_chd[-indices_entrenamiento, ]
#---- 10.1 Ajuste del modelo con training ----
modelo_train <- glm(Enfermedad ~ Edad + Colesterol + 
                      Presion + Tabaquismo + Diabetes + Obesidad,
                    data = train_data, family = binomial)
#---- 10.2 Prediccion con test set ----
test_data$prob <- predict(modelo_train, 
                          newdata = test_data, 
                          type = "response")
test_data$clasificacion <- ifelse(test_data$prob >= 0.5, 1, 0)
#---- 10.3 Matriz de confusion y metricas ----
matriz_test <- table(
  factor(test_data$Enfermedad, levels = c(0, 1)),
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
  rRecall = round(rRecall, 3),
  Specificity = round(Specificity, 3),
  F1_Score = round(F1_Score, 3)
)
print(Resultados_test)
#---- 10.4 Curva ROC test ----
library(pROC)
roc_test <- roc(test_data$Enfermedad, test_data$prob)
plot(roc_test, col = "blue", lwd = 2,
     main = "Curva ROC - Test set (CHD)")
abline(a = 0, b = 1, col = "gray", lty = 2)
auc(roc_test)

