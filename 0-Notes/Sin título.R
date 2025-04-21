set.seed(123)
n <- 500

# Variables numéricas
# Variables numéricas

age <- round(rnorm(n, mean = 54, sd = 9))
# Edad del paciente (en años). Promedio de 54 con una desviación estándar de 9.

sex <- rbinom(n, 1, 0.7)
# Sexo del paciente: 0 = mujer, 1 = hombre. Se asume 70% de hombres.

rest_bp <- round(rnorm(n, mean = 130, sd = 15))
# Presión arterial en reposo (en mm Hg). Promedio de 130 mm Hg.

chol <- round(rnorm(n, mean = 245, sd = 50))
# Nivel de colesterol sérico (en mg/dl). Promedio de 245.

fbs <- rbinom(n, 1, 0.15)
# Glucemia en ayunas: 1 = >120 mg/dl, 0 = <=120 mg/dl. Se asume ~15% de pacientes con glucosa alta.

restecg <- sample(0:2, n, replace = TRUE, prob = c(0.5, 0.4, 0.1))
# Resultados del electrocardiograma en reposo:
# 0 = normal, 1 = con anormalidad de la onda ST-T, 2 = hipertrofia ventricular izquierda probable.

max_hr <- round(rnorm(n, mean = 150, sd = 22))
# Frecuencia cardíaca máxima alcanzada durante el ejercicio.

exang <- rbinom(n, 1, 0.3)
# Angina inducida por ejercicio: 1 = sí, 0 = no. Se simula en 30% de los casos.

oldpeak <- round(runif(n, min = 0.0, max = 6.2), 1)
# Depresión del segmento ST inducida por el ejercicio en relación al reposo.

slope <- sample(1:3, n, replace = TRUE, prob = c(0.45, 0.4, 0.15))
# Pendiente del segmento ST durante el ejercicio:
# 1 = ascendente, 2 = plana, 3 = descendente.

ca <- sample(0:4, n, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.07, 0.03))
# Número de vasos principales coloreados por fluoroscopía (de 0 a 4).

# Variables categóricas
chest_pain <- sample(c("typical", "asymptomatic", "nonanginal", "nontypical"),
                     n, replace = TRUE, prob = c(0.25, 0.35, 0.25, 0.15))
# Resultado del test de talio:
# "normal" = flujo sanguíneo normal, "fixed" = defecto fijo, "reversable" = defecto reversible.
thal <- sample(c("normal", "fixed", "reversable"),
               n, replace = TRUE, prob = c(0.6, 0.2, 0.2))

# Modelo logit ficticio para simular probabilidad de AHD
logit_p <- -6 + 0.06*age + 0.02*chol + 0.03*rest_bp + 0.5*sex - 0.02*max_hr +
  0.4*fbs + 0.3*exang + 0.7*ca + 0.3*(slope == 2) - 0.5*(thal == "normal")

# Probabilidades y respuesta
p_hd <- 1 / (1 + exp(-logit_p))
ahd <- rbinom(n, 1, prob = p_hd)
ahd <- factor(ahd, levels = c(0, 1), labels = c("No", "Yes"))

# Construir data frame
heart_df <- data.frame(
  Age = age,
  Sex = sex,
  ChestPain = chest_pain,
  RestBP = rest_bp,
  Chol = chol,
  Fbs = fbs,
  RestECG = restecg,
  MaxHR = max_hr,
  ExAng = exang,
  Oldpeak = oldpeak,
  Slope = slope,
  Ca = ca,
  Thal = thal,
  AHD = ahd
)

# Vista previa
head(heart_df)
View(heart_df)

# Generación de Datos Simulados Estilo Heart Data

n <- 500

# Variables numéricas

age <- round(rnorm(n, mean = 54, sd = 9))
sex <- rbinom(n, 1, 0.7)
rest_bp <- round(rnorm(n, mean = 130, sd = 15))
chol <- round(rnorm(n, mean = 245, sd = 50))
fbs <- rbinom(n, 1, 0.15)
restecg <- sample(0:2, n, replace = TRUE, prob = c(0.5, 0.4, 0.1))
max_hr <- round(rnorm(n, mean = 150, sd = 22))
exang <- rbinom(n, 1, 0.3)
oldpeak <- round(runif(n, min = 0.0, max = 6.2), 1)
slope <- sample(1:3, n, replace = TRUE, prob = c(0.45, 0.4, 0.15))
ca <- sample(0:4, n, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.07, 0.03))
thal <- sample(c("normal", "fixed", "reversable"), n, replace = TRUE, prob = c(0.55, 0.25, 0.2))

linear_predictor <- -3 + 0.03 * age + 0.6 * sex + 0.02 * chol + 0.8 * exang + 0.5 * ca + ifelse(thal == "reversable", 1, 0)
prob_ahd <- 1 / (1 + exp(-linear_predictor))
AHD <- rbinom(n, 1, prob_ahd)

heart_data <- data.frame(age, sex, rest_bp, chol, fbs, restecg,
                         max_hr, exang, oldpeak, slope, ca, thal, AHD)
# Análisis Exploratorio

library(ggplot2)
library(dplyr)

heart_data %>%
  ggplot(aes(x = thal, fill = thal)) +
  geom_bar() +
  labs(title = "Distribución de la variable Thal", x = "Tipo de resultado en test de talio", y = "Frecuencia") +
  theme_minimal()

heart_data %>%
  ggplot(aes(x = as.factor(sex), fill = thal)) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de resultados de Thal por sexo", x = "Sexo (0 = Mujer, 1 = Hombre)", y = "Proporción") +
  theme_minimal()


# Modelos de Clasificación


modelo_logit <- glm(AHD ~ age + sex + chol + exang + ca + thal, data = heart_data, family = binomial(link = "logit"))
modelo_probit <- glm(AHD ~ age + sex + chol + exang + ca + thal, data = heart_data, family = binomial(link = "probit"))

summary(modelo_logit)
summary(modelo_probit)
```

# Comparación Gráfica de Curvas Logit y Probit

x_grid <- seq(min(heart_data$age), max(heart_data$age), length.out = 100)

pred_data <- data.frame(
  age = x_grid,
  sex = 1,
  chol = mean(heart_data$chol),
  exang = 1,
  ca = 2,
  thal = factor("reversable", levels = c("fixed", "normal", "reversable"))
)

p_logit <- predict(modelo_logit, newdata = pred_data, type = "response")
p_probit <- predict(modelo_probit, newdata = pred_data, type = "response")

plot(x_grid, p_logit, type = "l", col = "blue", lwd = 2, ylim = c(0,1),
     ylab = "Probabilidad estimada", xlab = "Edad", main = "Logit vs Probit")
lines(x_grid, p_probit, col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("Logit", "Probit"), col = c("blue", "red"),
       lty = c(1, 2), lwd = 2)


# Ajuste por Mínimos Cuadrados No Lineales (NLS)


library(minpack.lm)

heart_data$prob_AHD <- as.numeric(as.character(heart_data$AHD))
modelo_nls <- nlsLM(prob_AHD ~ 1 / (1 + exp(-(b0 + b1 * age))),
                    data = heart_data,
                    start = list(b0 = -5, b1 = 0.1))
summary(modelo_nls)


# Comparación Logit, Probit y NLS

p_nls <- predict(modelo_nls, newdata = pred_data)

plot(x_grid, p_logit, type = "l", col = "blue", lwd = 2, ylim = c(0,1),
     ylab = "Probabilidad estimada", xlab = "Edad", main = "Comparación de Modelos")
lines(x_grid, p_probit, col = "red", lwd = 2, lty = 2)
lines(x_grid, p_nls, col = "purple", lwd = 2, lty = 3)
legend("bottomright", legend = c("Logit", "Probit", "NLS"),
       col = c("blue", "red", "purple"), lty = c(1, 2, 3), lwd = 2)

# Bootstrap (N = 1500)


set.seed(1234)
n_boot <- 1500
boot_indices <- sample(1:nrow(heart_data), size = n_boot, replace = TRUE)
boot_data <- heart_data[boot_indices, ]

modelo_logit_bt <- glm(AHD ~ age + sex + chol + exang + ca + thal, data = boot_data, family = binomial(link = "logit"))
modelo_probit_bt <- glm(AHD ~ age + sex + chol + exang + ca + thal, data = boot_data, family = binomial(link = "probit"))
modelo_nls_bt <- nlsLM(as.numeric(as.character(AHD)) ~ 1 / (1 + exp(-(b0 + b1 * age))),
                       data = boot_data,
                       start = list(b0 = -5, b1 = 0.1))


# Comparación Gráfica con Bootstrap

x_grid_bt <- seq(min(boot_data$age), max(boot_data$age), length.out = 100)

boot_pred_data <- data.frame(
  age = x_grid_bt,
  sex = 1,
  chol = mean(boot_data$chol),
  exang = 1,
  ca = 2,
  thal = factor("reversable", levels = c("fixed", "normal", "reversable"))
)

p_logit_bt <- predict(modelo_logit_bt, newdata = boot_pred_data, type = "response")
p_probit_bt <- predict(modelo_probit_bt, newdata = boot_pred_data, type = "response")
p_nls_bt <- predict(modelo_nls_bt, newdata = boot_pred_data)

plot(x_grid_bt, p_logit_bt, type = "l", col = "blue", lwd = 2, ylim = c(0,1),
     ylab = "Probabilidad estimada", xlab = "Edad", main = "Logit vs Probit vs NLS (Bootstrap)")
lines(x_grid_bt, p_probit_bt, col = "red", lwd = 2, lty = 2)
lines(x_grid_bt, p_nls_bt, col = "purple", lwd = 2, lty = 3)
legend("bottomright", legend = c("Logit BT", "Probit BT", "NLS BT"),
       col = c("blue", "red", "purple"), lty = c(1, 2, 3), lwd = 2)
