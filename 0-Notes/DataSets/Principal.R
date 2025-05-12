#---- 1. SIMULACION DE DATOS DE CANCER ----
##---- 1.1 FIJANDO LA SEMILLA y DIRECTORIO DE TRABAJO ----
setwd("~/Documents/GitHub/DeepLearningReview/0-Notes/DataSets")
set.seed(123)
##---- 1.2 INICIALIZANDO LAS VARIABLES Y PARAMETROS ----
n <- 1000
edad <- round(rnorm(n, mean = 55, sd = 10)) # Edad al diagnóstico
tamano_tumor <- round(rnorm(n, mean = 30, sd = 10), 1) # en mm
progesterona <- round(rnorm(n, mean = 15, sd = 5), 1) # niveles en ng/ml
##---- 1.3 GENERACION DEL MODELO ----
###---- 1.3.1 MODELO LOGIT----
log_odds <- -4 + 0.03 * edad + 0.05 * tamano_tumor - 0.1 * progesterona
prob_sobrevivir <- 1 / (1 + exp(-log_odds)) # se calcula la sigmoide
sobrevivio <- rbinom(n, 1, prob_sobrevivir)
###---- 1.3.2 CREACION DATA FRAME ----
datos_cancer <- data.frame(
  edad = edad,
  tamano_tumor = tamano_tumor,
  progesterona = progesterona,
  sobrevivio = sobrevivio
)
###---- 1.3.3 CREACION DE ARCHIVO .csv ---- 
write.csv(datos_cancer, "datos_cancer_simulados.csv", row.names = FALSE)
View(datos_cancer)
head(datos_cancer)
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
#---- 3. EJEMPLO DE CANCER SEGUNDA PARTE ----
set.seed(2025)
library(dplyr)
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
  0.04 * (edad - 50) +
  0.02 * (bmi - 25) +             # efecto leve del IMC
  0.5  * menopausia_post +       # efecto positivo para postmenopáusicas
  0.03 * (tamano_tumor - 20) +
  0.3  * er_status -             # efecto protector por receptor de estrógeno
  0.4  * pr_status +
  0.8  * her2_status +
  1.2  * brca1_mut -
  0.05 * ki67 +
  0.6  * quimio +                  # quimioterapia reduce riesgo de recidiva
  0.5  * hormonoterapia -
  0.1 * (grado_histologico - 1)
prob_recidiva <- 1 / (1 + exp(-lp))
recidiva <- rbinom(n, 1, prob = prob_recidiva)
###---- 3.2.1 CREACION DATA FRAME ----
datos_cancer <- data.frame(
  Edad = edad,  IMC = bmi,
  Menopausia = menopausia,  Tamano_Tumor = tamano_tumor,
  Grado = grado_histologico,  ER = er_status,
  PR = pr_status,  HER2 = her2_status,
  BRCA1 = brca1_mut,  Ki67 = ki67,
  Quimio = quimio,  Hormonoterapia = hormonoterapia,
  Recidiva = recidiva
)
View(datos_cancer)
head(datos_cancer)
###---- 3.2.2 CREACION DE ARCHIVO .csv ---- 
write.csv(datos_cancer, "datos_simulados_cancer.csv", row.names = FALSE)
#---- 4. GENERACION DE GRAFICOS PARA DATOS DE CANCER ----
##---- 4.1 LLAMADO DE LIBRERIAS ----
library(ggplot2)
library(dplyr)
##---- 4.2 MODELO DE REGRESIÓN LOGÍSTICA: estimación MLE ----
modelo_cancer <- glm(Recidiva ~ Edad + Tamano_Tumor + IMC + Menopausia +
                       Grado + PR + ER + HER2 + BRCA1 + Ki67 + Quimio + Hormonoterapia,
              data = datos_cancer, family = binomial)
summary(modelo_cancer)

##---- 4.3 RAZONES DE MOMIOS E INTERVALOS DE CONFIANZA ----
exp_coef <- exp(coef(modelo_cancer))
exp_ci <- exp(confint(modelo_cancer))
##---- 4.4 RESUMEN DEL CALCULO DE MOMIOS ----
or_tabla <- data.frame(
  Variable = names(exp_coef),
  OR = round(exp_coef, 3),
  IC_inf = round(exp_ci[,1], 3),
  IC_sup = round(exp_ci[,2], 3)
)
print(or_tabla)
##---- 4.5 PREPARANDO LA GRAFICA ---
or_tabla_plot <- or_tabla[or_tabla$Variable != "(Intercept)", ]
or_tabla_plot$Variable <- factor(or_tabla_plot$Variable, 
                                 levels = rev(or_tabla_plot$Variable))
##---- 4.6 GENERACION DE LA GRAFICA ----
ggplot(or_tabla_plot, aes(x = OR, y = Variable)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Forest Plot del Modelo de Estudio de Cancer",
       x = "Odds Ratio (OR)", y = NULL) +
  theme_minimal()
##---- 4.7 GRAFICANDO PREDICCIONES ----
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
         color = "Recidiva real") +
    theme_minimal()
#---- 5. CALCULO DE LA LOG-VEROSIMILITUD ----  
loglik_manual <- function(beta, X, y) {
  eta <- X %*% beta
  p <- 1 / (1 + exp(-eta))
  sum(y * log(p) + (1 - y) * log(1 - p))
}
X <- model.matrix(modelo_cancer)
y <- datos_cancer$Recidiva
beta_hat <- coef(modelo_cancer)
loglik_manual(beta_hat, X, y)  # Debería ser cercano al logLik(modelo)
##---- 5.1 Función sigmoide ----
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}
##---- 5.2 Función de pérdida: log-verosimilitud negativa ----
log_likelihood <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  -sum(y * log(p + 1e-10) + (1 - y) * log(1 - p + 1e-10))  # pequeña corrección numérica
}
##---- 5.3 Gradiente de la pérdida ----
log_likelihood_grad <- function(beta, X, y) {
  p <- sigmoid(X %*% beta)
  t(X) %*% (p - y)
}
##---- 5.4 Gradiente Conjugado no lineal (Fletcher-Reeves simplificado) ----
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
  warning("No convergió")
  return(beta)
}
##---- 5.5 IMPLEMENTACION ----
X <- model.matrix(Recidiva ~ Edad + Tamano_Tumor + Grado + PR + HER2 + BRCA1 + Ki67 + Hormonoterapia, data = datos_cancer)
y <- datos_cancer$Recidiva
coef_conjugado <- gradiente_conjugado(X, y)
coef_glm <- coef(glm(Recidiva ~ Edad + Tamano_Tumor + Grado + PR + HER2 + BRCA1 + Ki67 + Hormonoterapia,
                     data = datos_cancer, family = binomial))
##---- 5.6 COMPARACION DE RESULTADOS ----
round(data.frame(
  Variable = names(coef_glm),
  Coef_GLM = coef_glm,
  Coef_GradConj = coef_conjugado
), 4)

#---- 6. NUEVO EJERCICIO DE SIMULACION ----
##---- 6.1 INICIALIZACION DE VARIABLES ----
set.seed(123)
n <- 500
edad <- round(runif(n, 25, 80))
tamano_tumor <- round(runif(n, 5, 50))
progesterona <- round(runif(n, 1, 100))
HER2 <- rbinom(n, 1, 0.25)
RE <- rbinom(n, 1, 0.6)
RP <- rbinom(n, 1, 0.55)
densidad_mamaria <- sample(1:4, n, replace = TRUE)

###---- 6.2. Coeficientes simulados----
log_odds <- -4 + 0.03 * edad + 0.05 * tamano_tumor - 0.1 * progesterona +
  0.8 * HER2 + 0.6 * RE + 0.5 * RP + 0.3 * densidad_mamaria
###---- 6.3. Probabilidad y diagnóstico ----
prob <- 1 / (1 + exp(-log_odds))
diagnostico <- rbinom(n, 1, prob)
###---- 6.4. Base de datos ----
datos <- data.frame(edad, tamano_tumor, progesterona, HER2, RE, RP, densidad_mamaria, diagnostico)
###---- 6.5. Modelo de regresión logística (MLE) ----
modelo <- glm(diagnostico ~ ., data = datos, family = binomial)
summary(modelo)
###---- 6.6. Comparar coeficientes verdaderos vs estimados----
coef_verdaderos <- c(-4, 0.03, 0.05, -0.1, 0.8, 0.6, 0.5, 0.3)
names(coef_verdaderos) <- names(coef(modelo))
comparacion <- data.frame(
  Coef_Verdadero = coef_verdaderos,
  Coef_Estimado = coef(modelo)
)
print(comparacion)
###---- 6.7. Visualizar con función sigmoide ----
##### Elegimos 2 variables: edad vs probabilidad estimada
datos$probabilidad_estimada <- predict(modelo, type = "response")

ggplot(datos, aes(x = edad, y = probabilidad_estimada, color = factor(diagnostico))) +
  geom_point(alpha = 0.6) +
  stat_function(fun = function(x) 1 / (1 + exp(-(-4 + 0.03 * x + 0.05 * 25 - 0.1 * 50))),
                color = "black", linetype = "dashed", size = 1.2) +
  labs(title = "Función Sigmoide Estimada vs Real",
       x = "Edad", y = "Probabilidad Estimada",
       color = "Diagnóstico") +
  theme_minimal()
