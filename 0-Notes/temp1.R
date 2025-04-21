# Fijar semilla para reproducibilidad
set.seed(123)

# Número de pacientes
n <- 1300

# Simulación de variables
patient_number <- 1:n
age <- round(runif(n, min = 20, max = 80))  # edades entre 20 y 80 años
tumor_size <- round(rnorm(n, mean = 2.5, sd = 1.0), 1)  # tamaño tumoral en cm
marker_A <- round(rnorm(n, mean = 1.0, sd = 0.3), 1)  # marcador A
marker_B <- round(rnorm(n, mean = 1.2, sd = 0.4), 1)  # marcador B

# Simulación de tipo de cáncer con probabilidad basada en las otras variables (regresión logística simplificada)
logit <- -5 + 0.04 * age + 0.8 * tumor_size + 1.2 * marker_B - 0.5 * marker_A
prob <- 1 / (1 + exp(-logit))
cancer_type <- rbinom(n, 1, prob)

# Crear data frame
datos_simulados <- data.frame(
  PatientNumber = patient_number,
  Age = age,
  TumorSize_cm = tumor_size,
  MarkerA = marker_A,
  MarkerB = marker_B,
  CancerType = cancer_type
)

# Visualizar los primeros registros
head(datos_simulados)

#---- SEGUNDA PARTE ----
# Simulación del conjunto de datos
set.seed(777)
n <- 1300

# Variables independientes
age <- round(runif(n, 20, 80))  # edad entre 20 y 80 años
tumor_size <- round(rnorm(n, 2.5, 1), 1)  # tumor size con media 2.5 cm
marker_A <- round(rnorm(n, 1.0, 0.3), 1)  # marcador A
marker_B <- round(rnorm(n, 1.2, 0.4), 1)  # marcador B

# Ecuación logística usando los coeficientes de la tabla
log_odds <- -3.126 + 0.032 * age + 0.732 * tumor_size + 1.348 * marker_A + 0.898 * marker_B
prob <- 1 / (1 + exp(-log_odds))
cancer_type <- rbinom(n, 1, prob)

# Data frame final
datos_cancer <- data.frame(
  PatientNumber = 1:n,
  Age = age,
  TumorSize_cm = tumor_size,
  MarkerA = marker_A,
  MarkerB = marker_B,
  CancerType = cancer_type
)

# Visualizar los primeros renglones
head(datos_cancer)

# (Opcional) Guardar como CSV
write.csv(datos_cancer, "datos_cancer_simulados.csv", row.names = FALSE)


#---- TERCERA PARTE ----
# Paquetes necesarios
if (!require(dplyr)) install.packages("dplyr")
if (!require(broom)) install.packages("broom")

library(dplyr)
library(broom)

set.seed(777)
n <- 1300

# Variables predictoras
age <- round(runif(n, 20, 80))
tumor_size <- round(rnorm(n, 2.5, 1), 1)
marker_A <- round(rnorm(n, 1.0, 0.3), 1)
marker_B <- round(rnorm(n, 1.2, 0.4), 1)

# Modelo logístico: misma estructura de la imagen
log_odds <- -3.126 + 0.032 * age + 0.732 * tumor_size + 1.348 * marker_A + 0.898 * marker_B
prob <- 1 / (1 + exp(-log_odds))
cancer_type <- rbinom(n, 1, prob)

# Construir el data frame
df <- data.frame(
  Age = age,
  TumorSize = tumor_size,
  MarkerA = marker_A,
  MarkerB = marker_B,
  CancerType = cancer_type
)

# Ajustar el modelo logístico
modelo <- glm(CancerType ~ Age + TumorSize + MarkerA + MarkerB, data = df, family = binomial)

# Resumen del modelo
summary(modelo)

# Crear tabla con coeficientes, OR y p-valores
resultados <- tidy(modelo) %>%
  mutate(
    OR = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error)
  )

print(resultados)
