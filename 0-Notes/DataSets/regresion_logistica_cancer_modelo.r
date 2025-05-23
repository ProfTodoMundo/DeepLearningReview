#---- 0. SEMILLA INICIAL Y LIBRERIAS---- 
set.seed(123)
##---- Librerias ----
library(pROC)
library(ggplot2)
library(broom)
library(dplyr)
# ---- 1. SIMULACION DE DATOS DE CANCER ----
n_8 <- 100;
n_5 <- 200; n_11 <- 200
n_3 <- 500; n_9 <- 500; n_10 <- 500
n_1 <- 1000; n_4 <- 1000;
n_2 <- 1300
##---- 1.1 VARIABLES A CONSIDERAR ----
###---- EDAD ----
edad_cancer_1  <- round(rnorm(n_1, mean = 55, sd = 10))
#edad_cancer_4  <- round(rnorm(n_4, mean = 55, sd = 10))
#edad_cancer_5  <- round(rnorm(n_5, mean = 55, sd = 10))
#edad_cancer_9  <- round(rnorm(n_9, mean = 55, sd = 10))
#edad_cancer_10 <- round(rnorm(n_10, mean = 55, sd = 10))
#edad_cancer_2  <- round(runif(n_2, 20, 80))
#edad_cancer_3  <- round(runif(n_3, 25, 80))
#edad_cancer_8  <- round(runif(n_8, 30, 80))
#edad_cancer_11 <- round(runif(n_11, 30, 80))
###---- TAMANHO DEL TUMOR ----
#tamano_tumor_1 <- round(rnorm(n_1, mean = 30,  sd = 10), 1)
tamano_tumor_2 <- round(rnorm(n_2, mean = 2.5, sd = 1), 1)
#tamano_tumor_4 <- round(rnorm(n_4, mean = 30,  sd = 10), 1)
#tamano_tumor_9 <- round(rnorm(n_9, mean = 2.5, sd = 1), 1)
#tamano_tumor_10 <- round(rnorm(n_10, mean = 2.5, sd = 1), 1)
#tamano_tumor_11 <- round(rnorm(n_11, mean = 25,  sd = 10), 1)
#tamano_tumor_3 <- round(runif(n_3, 5, 50))
###---- PROGESTERONA ----
progesterona_1 <- round(rnorm(n_1, mean = 15, sd = 5), 1)
#progesterona_4 <- round(rnorm(n_4, mean = 15, sd = 5), 1)
#progesterona_9 <- round(rnorm(n_9, mean = 15, sd = 5),1)
#progesterona_10 <- round(rnorm(n_10, mean = 15, sd = 5),1)
#progesterona_11 <- round(round(rnorm(n_11, mean = 15, sd = 5), 1))
#progesterona_3 <- round(runif(n_3, 1, 100))
###---- HER ----
#HER2_3  <- rbinom(n_3, 1, 0.25)
HER2_10 <- rbinom(n_10, 1, 0.3)
#HER2_9  <- rbinom(n_9, 1, 0.3)
###---- RE ----
RE_3  <- rbinom(n_3, 1, 0.6)
#RE_10 <- rbinom(n_10, 1, 0.6)
#RE_9  <- rbinom(n_9, 1, 0.6)
###---- RP ----
#RP_3  <- rbinom(n_3, 1, 0.55)
RP_9  <- rbinom(n_9, 1, 0.5)
#RP_10 <- rbinom(n_10, 1, 0.5)
###---- DENSIDAD MAMARIA ----
#densidad_mamaria_3  <- sample(1:4, n_3, replace = TRUE)
densidad_mamaria_9  <- rnorm(n_9, mean = 0.6, sd = 0.2)
#densidad_mamaria_10 <- rnorm(n_10, mean = 0.6, sd = 0.2)
###---- MARCADOR A ----
marker_A_2 <- round(rnorm(n_2, 1.0, 0.3), 1)
###---- MARCADOR B ----
marker_B_2 <- round(rnorm(n_2, 1.2, 0.4), 1)
###---- GEN CANCER ----
gen_cancer_5 <- rbinom(n_5, 1, prob = 0.4)
##---- 1.2 LOG ODDS ----

log_odds_cancer_1 <- -4 + 0.03 * edad_cancer_1 + 
  0.05 * tamano_tumor_1 - 0.1 * progesterona_1

log_odds_2 <- -3.126 + 0.032 * edad_cancer_2 + 0.732 * tamano_tumor_2 +
  1.348 * marker_A_2 + 0.898 * marker_B_2
  
log_odds_3 <- -4 + 0.03 * edad_cancer_3 + 0.05 * tamano_tumor_3 -
  0.1 * progesterona_3 +
  0.8 * HER2_3 + 0.6 * RE_3 + 0.5 * RP_3 +
  0.3 * densidad_mamaria_3

log_odds_cancer_4 <- -4 + 0.03 * edad_cancer_4 +
  0.05 * tamano_tumor_4 - 0.1 * progesterona_4

x_cancer_5 <- cbind(1, edad_cancer_5, gen_cancer_5)
beta_cancer_5 <- c(-5, 0.08, 1.2)
logit_cancer_5 <- x_cancer_5 %*% beta_cancer_5

b0_cancer_8 <- -6
b1_cancer_8 <- 0.1
log_odds_cancer_8 <- b0_cancer_8 + b1_cancer_8 * edad_cancer_8

log_odds_9 <- -4 + 0.03 * edad_cancer_9 + 0.05 * tamano_tumor_9 - 
  0.1 * progesterona_9 +0.8 * HER2_9 + 0.6 * RE_9 + 
  0.5 * RP_9 + 0.3 * densidad_mamaria_9

log_odds_10 <- -4 + 0.03 * edad_cancer_10 + 0.05 * tamano_tumor_10 -
  0.1 * progesterona_10 + 0.8 * HER2_10 + 0.6 * RE_10 +
  0.5 * RP_10 + 0.3 * densidad_mamaria_10

log_odds_11 <- -4 + 0.03 * edad_cancer_11 + 
  0.05 * tamano_tumor_11 - 0.1 * progesterona_11

##---- PROBABILIDAD SOBREVIVIR ----
prob_sobrevida_1 <- 1 / (1 + exp(-log_odds_cancer_1))
prob_2 <- 1 / (1 + exp(-log_odds_2))
prob_3 <- 1 / (1 + exp(-log_odds_3))
prob_sobrevida_4 <- 1 / (1 + exp(-log_odds_cancer_4))
p_cancer_5 <- 1 / (1 + exp(-logit_cancer_5))
prob_cancer_8 <- 1 / (1 + exp(-log_odds_cancer_8))
probabilidad_9 <- 1 / (1 + exp(-log_odds_9))
probabilidad_10 <- 1 / (1 + exp(-log_odds_10))
probabilidad_cancer_11 <- 1 / (1 + exp(-log_odds_11))
##---- SOBREVIVENCIA ----
sobrevivio_1 <- rbinom(n_1, 1, prob_sobrevida_1)
cancer_type_2 <- rbinom(n_2, 1, prob_2)
diagnostico_3 <- rbinom(n_3, 1, prob_3)
sobrevivio_4 <- rbinom(n_4, 1, prob_sobrevida_4)
y_cancer_5 <- rbinom(n_5, 1, p_cancer_5)
diagnostico_cancer_8 <- rbinom(n_8, 1, prob_cancer_8)
cancer_9 <- rbinom(n_9, 1, probabilidad_9)
cancer_10 <- rbinom(n_10, 1, probabilidad_10)
cancer_11 <- rbinom(n_11, 1, probabilidad_cancer_11)
##---- GENERACION DEL DATAFRAME ----

datos_cancer_1 <- data.frame(
  edad = edad_cancer_1,
  tamano_tumor = tamano_tumor_1,
  progesterona = progesterona_1,
  sobrevivio = sobrevivio_1)

df_2 <- data.frame(Age = age_2, TumorSize = tumor_size_2,
                 MarkerA = marker_A_2, MarkerB = marker_B_2,
                 CancerType = cancer_type_2)

datos_3 <- data.frame(edad_3, tamano_tumor_3, progesterona_3,
                    HER2_3, RE_3, RP_3, densidad_mamaria_3, 
                    diagnostico_3)
					
datos_cancer_4 <- data.frame(
  edad = edad_cancer_4,
  tamano_tumor = tamano_tumor_4,
  progesterona = progesterona_4,
  sobrevivio = sobrevivio_4)
					
datos_cancer_8 <- data.frame(
  Edad = edad_cancer_8, Diagnostico = diagnostico_cancer_8)

datos_9 <- data.frame(
  edad_9, tamano_tumor_9, progesterona_9, HER2_9, RE_9, RP_9,
  densidad_mamaria_9, cancer_9 = factor(cancer_9)
)

datos_10 <- data.frame(
  edad_10, tamano_tumor_10, progesterona_10, HER2_10,
  RE_10, RP_10, densidad_mamaria_10, cancer_10 = factor(cancer_10))

datos_cancer_11 <- data.frame(edad_11, tamano_tumor_11,
                              progesterona_11, cancer_11)
## ---- AJUSTE DEL MODELO ----
modelo_cancer_1 <- glm(sobrevivio ~ edad + tamano_tumor + progesterona, 
                     data = datos_cancer_1, family = binomial)
summary(modelo_cancer_1)

modelo_2 <- glm(CancerType ~ Age + TumorSize + MarkerA + MarkerB, 
                data = df_2, family = binomial)
summary(modelo_2)				
modelo_3 <- glm(diagnostico_3 ~ ., data = datos_3, family = binomial)
summary(modelo_3)

modelo_cancer_4 <- glm(sobrevivio ~ edad + tamano_tumor +
                         progesterona, 
                     data = datos_cancer_4, family = binomial)
summary(modelo_cancer_4)

modelo_cancer_8 <- glm(diagnostico_cancer_8 ~ Edad_8, family = binomial, data = datos_cancer_8)
summary(modelo_cancer_8)

modelo_9 <- glm(cancer_9 ~ edad_9 + tamano_tumor_9 +
                  progesterona_9 + HER2_9 + RE_9 +
                  RP_9 + densidad_mamaria_9,
                data = datos_9, family = binomial())
summary(modelo_cancer_9)
modelo_10 <- glm(cancer_10 ~ edad_10 + tamano_tumor_10 +
                   progesterona_10 + HER2_10 + RE_10 +
                   RP_10 + densidad_mamaria_10,
                 data = datos_10, family = binomial())
summary(modelo_10)

modelo_cancer_11 <- glm(cancer_11 ~ edad_11 +
                          tamano_tumor_11 +
                          progesterona_11, 
                        family = binomial, 
                        data = datos_cancer_11)
summary(modelo_cancer_11)
datos_cancer_1$prob <- predict(modelo_cancer_1, type = "response")
##---- GRAFICA ----
ggplot(datos_cancer_1, aes(x = edad, y = prob)) +
  geom_point(aes(color = factor(sobrevivio)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Sobrevivir a 5 años vs Edad",
       y = "Probabilidad estimada", color = "Sobrevivió") +
  theme_minimal()

prob_pred_2 <- predict(modelo_2, type = "response")
roc_obj_2 <- roc(df_2$CancerType, prob_pred_2)
plot(roc_obj_2, col = "blue", lwd = 2, 
     main = "Curva ROC - Modelo cáncer simulado")
abline(a = 0, b = 1, col = "gray", lty = 2)

ggplot(data.frame(prob = prob_pred_2), aes(x = prob)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "Histograma de probabilidades predichas", x = "Probabilidad estimada", y = "Frecuencia")

coef_data_2 <- tidy(modelo_2, conf.int = TRUE, 
                    conf.level = 0.95, exponentiate = TRUE)
coef_data_2 <- coef_data_2[coef_data_2$term != "(Intercept)", ]
ggplot(coef_data_2, aes(x = term, y = estimate, 
                        ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "darkred") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Odds Ratio con IC95%", x = "Variable", y = "OR (exp(β))") +
  theme_minimal()


coef_verdaderos_3 <- c(-4, 0.03, 0.05, -0.1, 0.8, 0.6, 0.5, 0.3)
names(coef_verdaderos_3) <- names(coef(modelo_3))
comparacion_3 <- data.frame(
  Coef_Verdadero_3 = coef_verdaderos_3,
  Coef_Estimado_3 = coef(modelo_3)
)
print(comparacion_3)

exp(cbind(OR = coef(modelo_cancer_4), confint(modelo_cancer_4)))
datos_cancer_4$prob <- predict(modelo_cancer_4, type = "response")
ggplot(datos_cancer_4, aes(x = edad, y = prob)) +
  geom_point(aes(color = factor(sobrevivio)), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Probabilidad de Sobrevivir a 5 años vs Edad",
       y = "Probabilidad estimada", color = "Sobrevivió") +
  theme_minimal()

coef_verdaderos_9 <- c("(Intercept)" = -4, "edad" = 0.03, "tamano_tumor" = 0.05,
                       "progesterona" = -0.1, "HER2" = 0.8, "RE" = 0.6,
                       "RP" = 0.5, "densidad_mamaria" = 0.3)
coef_estimados_9 <- coef(modelo_9)
comparacion_9 <- data.frame(
  Coeficiente = names(coef_verdaderos_9),
  Verdadero = coef_verdaderos_9,
  Estimado = round(coef_estimados_9[names(coef_verdaderos_9)], 4),
  Diferencia = round(coef_estimados_9[names(coef_verdaderos_9)] - coef_verdaderos_9, 4)
)
print(comparacion_9)
datos_9$prob_predicha <- predict(modelo_9, type = "response")
ggplot(datos_9, aes(x = edad_9, y = prob_predicha, color = cancer_9)) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = FALSE, color = "black") +
  labs(title = "Probabilidad predicha vs edad",
       y = "Probabilidad predicha", x = "Edad")

coef_verdaderos_10 <- c("(Intercept)" = -4, "edad" = 0.03, 
                        "tamano_tumor" = 0.05, "progesterona" = -0.1,
                        "HER2" = 0.8, "RE" = 0.6, "RP" = 0.5,
                        "densidad_mamaria" = 0.3)
coef_estimados_10 <- coef(modelo_10)
comparacion_10 <- data.frame(
  Coeficiente = names(coef_verdaderos_10),
  Verdadero = coef_verdaderos_10,
  Estimado = round(coef_estimados_10[names(coef_verdaderos_10)], 4),
  Diferencia = round(coef_estimados_10[names(coef_verdaderos_10)] -
                       coef_verdaderos_10, 4))
print(comparacion_10)
datos_10$prob_predicha <- predict(modelo_10, type = "response")
ggplot(datos_10, aes(x = edad_10, y = prob_predicha, color = cancer_10)) +
  geom_point(alpha = 0.6) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = FALSE, color = "black") +
  labs(title = "Probabilidad predicha vs edad",
       y = "Probabilidad predicha", x = "Edad")
ggplot(datos_cancer_11, aes(x = edad_11,
                            y = probabilidad_cancer_11,
                            color = factor(cancer_11))) +
  geom_point(alpha = 0.6) +
  stat_function(fun = function(x) 1 / (1 + exp(-(coef(modelo_cancer_11)[1] + coef(modelo_cancer_11)[2]*x +
                                                   coef(modelo_cancer_11)[3]*mean(tamano_tumor_11) +
                                                   coef(modelo_cancer_11)[4]*mean(progesterona_11)))), 
                color = "black", linetype = "dashed") +
  labs(title = "Regresión logística: Cáncer de mama", y = "Probabilidad estimada", color = "Diagnóstico")
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


