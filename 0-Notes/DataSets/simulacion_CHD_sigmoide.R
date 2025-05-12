
# Simulación de datos para enfermedad cardíaca (CHD)
set.seed(123)
n <- 200
AGE <- rnorm(n, mean = 55, sd = 10)
CHOL <- rnorm(n, mean = 220, sd = 30)
SMOKING <- rbinom(n, 1, prob = 0.4)

# Simulación del resultado binario CHD
lin_pred <- -6 + 0.05 * AGE + 0.03 * CHOL + 1.5 * SMOKING
prob_CHD <- 1 / (1 + exp(-lin_pred))
CHD <- rbinom(n, 1, prob_CHD)

data_chd <- data.frame(AGE, CHOL, SMOKING, CHD)

# Ajuste del modelo logístico
modelo_chd <- glm(CHD ~ AGE + CHOL + SMOKING, data = data_chd, family = binomial)

# Predicción del valor lineal y la probabilidad
xbeta <- predict(modelo_chd, type = "link")
probabilidades <- predict(modelo_chd, type = "response")

# Gráfica de la sigmoide con clasificación de puntos
x_vals <- seq(min(xbeta) - 1, max(xbeta) + 1, length.out = 500)
y_vals <- 1 / (1 + exp(-x_vals))

# Plot
plot(x_vals, y_vals, type = "l", lwd = 2, col = "blue",
     xlab = expression(X*beta), ylab = "Probabilidad", main = "Curva sigmoide con clasificación")
abline(h = 0.5, lty = 2, col = "red")
abline(v = 0, lty = 2, col = "gray")

# Puntos observados
points(xbeta[data_chd$CHD == 0], data_chd$CHD[data_chd$CHD == 0], col = "darkred", pch = 4)
points(xbeta[data_chd$CHD == 1], data_chd$CHD[data_chd$CHD == 1], col = "darkgreen", pch = 16)

# Leyenda
legend("bottomright", legend = c("Sigmoide", "CHD = 0", "CHD = 1"),
       col = c("blue", "darkred", "darkgreen"), pch = c(NA, 4, 16), lty = c(1, NA, NA), lwd = c(2, NA, NA))
