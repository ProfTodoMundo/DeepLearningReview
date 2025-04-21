
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
