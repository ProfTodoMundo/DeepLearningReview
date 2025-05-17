
# Paquetes necesarios
if (!require(pROC)) install.packages("pROC")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(broom)) install.packages("broom")

library(pROC)
library(ggplot2)
library(broom)

# Simulación de datos (igual que antes)
set.seed(777)
n <- 1300
age <- round(runif(n, 20, 80))
tumor_size <- round(rnorm(n, 2.5, 1), 1)
marker_A <- round(rnorm(n, 1.0, 0.3), 1)
marker_B <- round(rnorm(n, 1.2, 0.4), 1)
log_odds <- -3.126 + 0.032 * age + 0.732 * tumor_size + 1.348 * marker_A + 0.898 * marker_B
prob <- 1 / (1 + exp(-log_odds))
cancer_type <- rbinom(n, 1, prob)
df <- data.frame(Age = age, TumorSize = tumor_size, MarkerA = marker_A, MarkerB = marker_B, CancerType = cancer_type)

# Modelo
modelo <- glm(CancerType ~ Age + TumorSize + MarkerA + MarkerB, data = df, family = binomial)

# -----------------------------
# Gráfico 1: Curva ROC
# -----------------------------
prob_pred <- predict(modelo, type = "response")
roc_obj <- roc(df$CancerType, prob_pred)
png("curva_roc.png", width = 800, height = 600)
plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC - Modelo cáncer simulado")
abline(a = 0, b = 1, col = "gray", lty = 2)
dev.off()

# -----------------------------
# Gráfico 2: Histograma de probabilidades predichas
# -----------------------------
png("hist_probabilidades.png", width = 800, height = 600)
ggplot(data.frame(prob = prob_pred), aes(x = prob)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "Histograma de probabilidades predichas", x = "Probabilidad estimada", y = "Frecuencia")
dev.off()

# -----------------------------
# Gráfico 3: Efectos de los coeficientes con IC95%
# -----------------------------
coef_data <- tidy(modelo, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)
coef_data <- coef_data[coef_data$term != "(Intercept)", ]

png("efectos_logisticos.png", width = 800, height = 600)
ggplot(coef_data, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "darkred") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Odds Ratio con IC95%", x = "Variable", y = "OR (exp(β))") +
  theme_minimal()
dev.off()


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



#---- 4. NUEVO EJERCICIO DE SIMULACION ----
##---- 4.1 INICIALIZACION DE VARIABLES ----
set.seed(123)
n <- 500
edad <- round(runif(n, 25, 80))
tamano_tumor <- round(runif(n, 5, 50))
progesterona <- round(runif(n, 1, 100))
HER2 <- rbinom(n, 1, 0.25)
RE <- rbinom(n, 1, 0.6)
RP <- rbinom(n, 1, 0.55)
densidad_mamaria <- sample(1:4, n, replace = TRUE)
###---- 4.2. Coeficientes simulados----
log_odds <- -4 + 0.03 * edad + 0.05 * tamano_tumor - 0.1 * progesterona +
  0.8 * HER2 + 0.6 * RE + 0.5 * RP + 0.3 * densidad_mamaria
###---- 4.3. Probabilidad y diagnóstico ----
prob <- 1 / (1 + exp(-log_odds))
diagnostico <- rbinom(n, 1, prob)
###---- 4.4. Base de datos ----
datos <- data.frame(edad, tamano_tumor, progesterona,
                    HER2, RE, RP, densidad_mamaria, 
                    diagnostico)
###---- 4.5. Modelo de regresión logística (MLE) ----
modelo <- glm(diagnostico ~ ., data = datos, family = binomial)
summary(modelo)
###---- 4.6. Comparar coeficientes verdaderos vs estimados----
coef_verdaderos <- c(-4, 0.03, 0.05, -0.1, 0.8, 0.6, 0.5, 0.3)
names(coef_verdaderos) <- names(coef(modelo))
comparacion <- data.frame(
  Coef_Verdadero = coef_verdaderos,
  Coef_Estimado = coef(modelo)
)
print(comparacion)
###---- 4.7. Visualizar con función sigmoide ----
datos$probabilidad_estimada <- predict(modelo, type = "response")
ggplot(datos, aes(x = edad, y = probabilidad_estimada, color = factor(diagnostico))) +
  geom_point(alpha = 0.6) +
  stat_function(fun = function(x) 1 / (1 + exp(-(-4 + 0.03 * x + 0.05 * 25 - 0.1 * 50))),
                color = "black", linetype = "dashed", size = 1.2) +
  labs(title = "Función Sigmoide Estimada vs Real",
       x = "Edad", y = "Probabilidad Estimada",
       color = "Diagnóstico") +
  theme_minimal()

write.csv(datos_cancer, "datos_cancer_simulados.csv", row.names = FALSE)
View(datos_cancer)
head(datos_cancer)
