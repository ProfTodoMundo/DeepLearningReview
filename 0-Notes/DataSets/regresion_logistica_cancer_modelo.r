#---- 0. SEMILLA INICIAL Y LIBRERIAS---- 
set.seed(123)
##---- Librerias ----
library(pROC)
library(ggplot2)
library(broom)
library(dplyr)
# ---- 1. SIMULACION DE DATOS DE CANCER ----
n_1 <- 1000
n_2 <- 1300
n_3 <- 500
n_4 <- 1000
n_5 <- 200
n_8 <- 100
n_9 <- 500
n_10 <- 500
n_11 <- 200
#---- VARIABLES A CONSIDERAR ----
edad_cancer_1 <- round(rnorm(n_1, mean = 55, sd = 10))
tamano_tumor_1 <- round(rnorm(n_1, mean = 30, sd = 10), 1)
progesterona_1 <- round(rnorm(n_1, mean = 15, sd = 5), 1)

age_2 <- round(runif(n_2, 20, 80))
tumor_size_2 <- round(rnorm(n_2, 2.5, 1), 1)
marker_A_2 <- round(rnorm(n_2, 1.0, 0.3), 1)
marker_B_2 <- round(rnorm(n_2, 1.2, 0.4), 1)


edad_3 <- round(runif(n_3, 25, 80))
tamano_tumor_3 <- round(runif(n_3, 5, 50))
progesterona_3 <- round(runif(n_3, 1, 100))
HER2_3 <- rbinom(n_3, 1, 0.25)
RE_3 <- rbinom(n_3, 1, 0.6)
RP_3 <- rbinom(n_3, 1, 0.55)
densidad_mamaria_3 <- sample(1:4, n_3, replace = TRUE)


edad_cancer_4 <- round(rnorm(n_4, mean = 55, sd = 10))
tamano_tumor_4 <- round(rnorm(n_4, mean = 30, sd = 10), 1)
progesterona_4 <- round(rnorm(n_4, mean = 15, sd = 5), 1)

edad_cancer_5 <- rnorm(n_5, mean = 55, sd = 10)
gen_cancer_5 <- rbinom(n_5, 1, prob = 0.4)
x_cancer_5 <- cbind(1, edad_cancer_5, gen_cancer_5)
beta_cancer_5 <- c(-5, 0.08, 1.2)

edad_cancer_8 <- runif(n_8, 30, 80)
b0_cancer_8 <- -6
b1_cancer_8 <- 0.1


edad_9 <- rnorm(n_9, mean = 55, sd = 10)
tamano_tumor_9 <- rnorm(n_9, mean = 2.5, sd = 1)
progesterona_9 <- rnorm(n_9, mean = 15, sd = 5)
HER2_9 <- rbinom(n_9, 1, 0.3)
RE_9 <- rbinom(n_9, 1, 0.6)
RP_9 <- rbinom(n_9, 1, 0.5)
densidad_mamaria_9 <- rnorm(n_9, mean = 0.6, sd = 0.2)


edad_10 <- rnorm(n_10, mean = 55, sd = 10)
tamano_tumor_10 <- rnorm(n_10, mean = 2.5, sd = 1)
progesterona_10 <- rnorm(n_10, mean = 15, sd = 5)
HER2_10 <- rbinom(n_10, 1, 0.3)
RE_10 <- rbinom(n_10, 1, 0.6)
RP_10 <- rbinom(n_10, 1, 0.5)
densidad_mamaria_10 <- rnorm(n_10, mean = 0.6, sd = 0.2)

edad_11 <- round(runif(n_11, 30, 80))
tamano_tumor_11 <- round(rnorm(n_11, mean = 25, sd = 10), 1)
progesterona_11 <- round(rnorm(n_11, mean = 15, sd = 5), 1)

#---- LOG ODDS ----

log_odds_cancer_1 <- -4 + 0.03 * edad_cancer_1 + 
  0.05 * tamano_tumor_1 - 0.1 * progesterona_1

log_odds_2 <- -3.126 + 0.032 * age_2 + 0.732 * tumor_size_2 +
  1.348 * marker_A_2 + 0.898 * marker_B_2
  
log_odds_3 <- -4 + 0.03 * edad_3 + 0.05 * tamano_tumor_3 -
  0.1 * progesterona_3 +
  0.8 * HER2_3 + 0.6 * RE_3 + 0.5 * RP_3 +
  0.3 * densidad_mamaria_3

log_odds_cancer_4 <- -4 + 0.03 * edad_cancer_4 +
  0.05 * tamano_tumor_4 - 0.1 * progesterona_4

logit_cancer_5 <- x_cancer_5 %*% beta_cancer_5

log_odds_cancer_8 <- b0_cancer_8 + b1_cancer_8 * edad_cancer_8

log_odds_9 <- -4 + 0.03 * edad_9 + 0.05 * tamano_tumor_9 - 
  0.1 * progesterona_9 +0.8 * HER2_9 + 0.6 * RE_9 + 
  0.5 * RP_9 + 0.3 * densidad_mamaria_9

log_odds_10 <- -4 + 0.03 * edad_10 + 0.05 * tamano_tumor_10 -
  0.1 * progesterona_10 + 0.8 * HER2_10 + 0.6 * RE_10 +
  0.5 * RP_10 + 0.3 * densidad_mamaria_10

log_odds_11 <- -4 + 0.03 * edad_11 + 
  0.05 * tamano_tumor_11 - 0.1 * progesterona_11

#---- PROBABILIDAD SOBREVIVIR ----
prob_sobrevida_1 <- 1 / (1 + exp(-log_odds_cancer_1))
prob_2 <- 1 / (1 + exp(-log_odds_2))
prob_3 <- 1 / (1 + exp(-log_odds_3))
prob_sobrevida_4 <- 1 / (1 + exp(-log_odds_cancer_4))
p_cancer_5 <- 1 / (1 + exp(-logit_cancer_5))
prob_cancer_8 <- 1 / (1 + exp(-log_odds_cancer_8))
probabilidad_9 <- 1 / (1 + exp(-log_odds_9))
probabilidad_10 <- 1 / (1 + exp(-log_odds_10))
probabilidad_cancer_11 <- 1 / (1 + exp(-log_odds_11))
#---- SOBREVIVENCIA ----
sobrevivio_1 <- rbinom(n_1, 1, prob_sobrevida_1)
cancer_type_2 <- rbinom(n_2, 1, prob_2)
diagnostico_3 <- rbinom(n_3, 1, prob_3)
sobrevivio_4 <- rbinom(n_4, 1, prob_sobrevida_4)
y_cancer_5 <- rbinom(n_5, 1, p_cancer_5)
diagnostico_cancer_8 <- rbinom(n_8, 1, prob_cancer_8)
cancer_9 <- rbinom(n_9, 1, probabilidad_9)
cancer_10 <- rbinom(n_10, 1, probabilidad_10)
cancer_11 <- rbinom(n_11, 1, probabilidad_cancer_11)
#---- GENERACION DEL DATAFRAME ----

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
# ---- AJUSTE DEL MODELO ----
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
## ---- GRAFICA DE SIGMOIDE Y EL MODELO ----
datos_cancer_1$prob <- predict(modelo_cancer_1, type = "response")
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


