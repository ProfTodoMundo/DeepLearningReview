# Cargar paquetes
library(ggplot2)
library(dplyr)
library(minpack.lm)

# Simulación de datos
set.seed(123)
n <- 500

# Variables numéricas simuladas
age <- round(rnorm(n, mean = 54, sd = 9))
sex <- rbinom(n, 1, 0.7)
rest_bp <- round(rnorm(n, mean = 130, sd = 15))x
chol <- round(rnorm(n, mean = 245, sd = 50))
fbs <- rbinom(n, 1, 0.15)
restecg <- sample(0:2, n, replace = TRUE, prob = c(0.5, 0.4, 0.1))
max_hr <- round(rnorm(n, mean = 150, sd = 22))
exang <- rbinom(n, 1, 0.3)
oldpeak <- round(runif(n, min = 0.0, max = 6.2), 1)
slope <- sample(1:3, n, replace = TRUE, prob = c(0.45, 0.4, 0.15))
ca <- sample(0:4, n, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.07, 0.03))
chest_pain <- sample(c("typical", "asymptomatic", "nonanginal", "nontypical"),
                     n, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15))
thal <- sample(c("normal", "fixed", "reversable"),
               n, replace = TRUE, prob = c(0.6, 0.2, 0.2))

# Modelo logit ficticio
logit_p <- -6 + 0.06*age + 0.02*chol + 0.03*rest_bp + 0.5*sex - 0.02*max_hr +
  0.4*fbs + 0.3*exang + 0.7*ca + 0.3*(slope == 2) - 0.5*(thal == "normal")
p_hd <- 1 / (1 + exp(-logit_p))
ahd <- factor(rbinom(n, 1, prob = p_hd), levels = c(0, 1), labels = c("No", "Yes"))

# Base de datos simulada
heart_df <- data.frame(
  Age = age, Sex = sex, ChestPain = chest_pain,
  RestBP = rest_bp, Chol = chol, Fbs = fbs,
  RestECG = restecg, MaxHR = max_hr, ExAng = exang,
  Oldpeak = oldpeak, Slope = slope, Ca = ca, Thal = thal, AHD = ahd
)

# Ajuste de modelos logit y probit
modelo_logit <- glm(AHD ~ Age + Sex + Chol + ExAng + Ca + Thal, data = heart_df, family = binomial("logit"))
modelo_probit <- glm(AHD ~ Age + Sex + Chol + ExAng + Ca + Thal, data = heart_df, family = binomial("probit"))

# Predicción sobre valores controlados
x_grid <- seq(min(heart_df$Age), max(heart_df$Age), length.out = 100)
pred_data <- data.frame(
  Age = x_grid,
  Sex = 0,
  Chol = 180,
  ExAng = 0,
  Ca = 0,
  Thal = factor("reversable", levels = levels(heart_df$Thal))
)

# Probabilidades
p_logit <- predict(modelo_logit, newdata = pred_data, type = "response")
p_probit <- predict(modelo_probit, newdata = pred_data, type = "response")

# Gráfico comparativo
plot(x_grid, p_logit, type = "l", col = "blue", ylim = c(0, 1), lwd = 2,
     ylab = "Probabilidad", xlab = "Edad", main = "Logit vs Probit")
lines(x_grid, p_probit, col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("Logit", "Probit"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)
