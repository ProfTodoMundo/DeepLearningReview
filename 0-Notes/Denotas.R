set.seed(999)

# Age of the patients
age <- abs(rnorm(n = 900, mean = 67, sd = 14))

# Socioeconomic status of the patients
Socioeconomic <- factor(rbinom(n = 900, size = 1, prob = 0.6), labels = c("Bueno", "Suficiente"))

# BMI of the patients
bmi <- abs(round(rnorm(n = 900, mean = 25, sd = 2.5), 1))

# LAC of the patients
lac <- abs(round(rnorm(n = 900, mean = 5, sd = 3), 1))

# Gender of the patients
gender <- factor(rbinom(n = 900, size = 1, prob = 0.6), labels = c("Hombre", "Mujer"))

# WBC scores of the patients
wbc <- abs(round(rnorm(n = 900, mean = 10, sd = 3), 1))

# HB of the patients
hb <- abs(round(rnorm(n = 900, mean = 120, sd = 40)))

# Calculating z value and probability
z <- 0.1 * age - 0.02 * hb + lac - 10; 
pr <- 1 / (1 + exp(-z))

# Generating binary outcome (mortality)
y <- rbinom(900, 1, pr)
mort <- factor(y, labels = c("Vivo", "Fallecido"))

# Creating data frame
data <- data.frame(age, gender, lac, wbc, Socioeconomic, bmi, hb, mort)
View(data)


# Análisis univariado para edad
univariate.age <- glm(mort ~ age, family = binomial, data = data)
summary(univariate.age)

# Análisis univariado para estatus socioeconómico
univariate.socioeconomic <- glm(mort ~ Socioeconomic, family = binomial, data = data)
summary(univariate.socioeconomic)

# Análisis multivariado
model1 <- glm(mort ~ lac + hb + wbc + age + bmi + Socioeconomic + gender, 
              family = binomial, data = data)
summary(model1)

# Crear el modelo 2 quitando las variables no significativas
model2 <- glm(mort ~ lac + hb + age + bmi + gender, family = binomial, data = data)
summary(model2)


# Instalar y cargar la librería lmtest si no está instalada
# install.packages("lmtest")
library(lmtest)

# Comparar los dos modelos con test de razón de verosimilitud
lrtest(model1, model2)

# Ajustamos el modelo
model <- glm(mort ~ lac + hb + age + bmi, family = binomial, data = data)

# Calculamos el logit
logit <- log(predict(model, type = "response") / (1 - predict(model, type = "response")))

# Creamos el layout de 2x2
par(mfrow = c(2, 2))

# Dibujamos los gráficos
plot(data$age, logit, main = "Edad", xlab = "Edad", ylab = "log(pr/(1 - pr))")
lines(lowess(data$age, logit), col = "black")

plot(data$lac, logit, main = "lac", xlab = "lac", ylab = "log(pr/(1 - pr))")
lines(lowess(data$lac, logit), col = "black")

plot(data$hb, logit, main = "hb", xlab = "hb", ylab = "log(pr/(1 - pr))")
lines(lowess(data$hb, logit), col = "black")

plot(data$bmi, logit, main = "bmi", xlab = "bmi", ylab = "log(pr/(1 - pr))")
lines(lowess(data$bmi, logit), col = "black")


# Modelo extendido (puedes cambiar las variables que desees evaluar)
model_ext <- glm(mort ~ age + lac + hb + bmi + wbc, family = binomial, data = data)

# Calcular logit de las probabilidades predichas
logit <- log(predict(model_ext, type = "response") / (1 - predict(model_ext, type = "response")))

# Listado de variables continuas a evaluar
variables <- c("age", "lac", "hb", "bmi", "wbc")

# Preparar layout dinámico según el número de variables
par(mfrow = c(ceiling(length(variables)/2), 2))

# Generar un gráfico para cada variable
for (var in variables) {
  x <- data[[var]]
  plot(x, logit,
       main = var,
       xlab = var,
       ylab = "log(pr / (1 - pr))",
       pch = 20, col = "darkgray")
  lines(lowess(x, logit), col = "blue", lwd = 2)
}



