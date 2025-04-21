
# Análisis de regresión logística con datos simulados de cáncer

set.seed(777)
n <- 1300

age <- round(runif(n, 20, 80))
tumor_size <- round(rnorm(n, 2.5, 1), 1)
marker_A <- round(rnorm(n, 1.0, 0.3), 1)
marker_B <- round(rnorm(n, 1.2, 0.4), 1)

log_odds <- -3.126 + 0.032 * age + 0.732 * tumor_size + 1.348 * marker_A + 0.898 * marker_B
prob <- 1 / (1 + exp(-log_odds))
cancer_type <- rbinom(n, 1, prob)

df <- data.frame(
  Age = age,
  TumorSize = tumor_size,
  MarkerA = marker_A,
  MarkerB = marker_B,
  CancerType = cancer_type
)

modelo <- glm(CancerType ~ Age + TumorSize + MarkerA + MarkerB, data = df, family = binomial)
summary(modelo)

# Calcular OR y estadístico de Wald
coef <- coef(summary(modelo))
OR <- exp(coef[,1])
Wald <- coef[,1] / coef[,2]
tabla <- cbind(coef, "OR" = OR, "Wald" = Wald)
print(round(tabla, 4))
