
# ============================================
# Simulación y estimación MLE para regresión logística
# ============================================
set.seed(123)

# Simulación de datos: Cáncer
n <- 200
edad_cancer <- rnorm(n, mean = 55, sd = 10)
gen_cancer <- rbinom(n, 1, prob = 0.4)
x_cancer <- cbind(1, edad_cancer, gen_cancer)
beta_cancer <- c(-5, 0.08, 1.2)
logit_cancer <- x_cancer %*% beta_cancer
p_cancer <- 1 / (1 + exp(-logit_cancer))
y_cancer <- rbinom(n, 1, p_cancer)

# Simulación de datos: CHD
edad_chd <- rnorm(n, mean = 60, sd = 12)
presion_chd <- rnorm(n, mean = 140, sd = 20)
x_chd <- cbind(1, edad_chd, presion_chd)
beta_chd <- c(-6, 0.05, 0.03)
logit_chd <- x_chd %*% beta_chd
p_chd <- 1 / (1 + exp(-logit_chd))
y_chd <- rbinom(n, 1, p_chd)

# ============================================
# Función de verosimilitud para MLE
# ============================================
log_likelihood <- function(beta, X, y) {
  p <- 1 / (1 + exp(-X %*% beta))
  ll <- sum(y * log(p) + (1 - y) * log(1 - p))
  return(-ll)  # Negativa porque optim minimiza
}

# Gradiente de la log-verosimilitud
log_likelihood_grad <- function(beta, X, y) {
  p <- 1 / (1 + exp(-X %*% beta))
  grad <- -t(X) %*% (y - p)
  return(as.vector(grad))
}

# Estimación MLE para cáncer
mle_cancer <- optim(par = c(0, 0, 0),
                    fn = log_likelihood,
                    gr = log_likelihood_grad,
                    method = "BFGS",
                    X = x_cancer,
                    y = y_cancer,
                    control = list(fnscale = 1))

# Estimación MLE para CHD
mle_chd <- optim(par = c(0, 0, 0),
                 fn = log_likelihood,
                 gr = log_likelihood_grad,
                 method = "BFGS",
                 X = x_chd,
                 y = y_chd,
                 control = list(fnscale = 1))

# Resultados
cat("\n--- Resultados MLE Cáncer ---\n")
print(mle_cancer$par)
cat("\n--- Resultados MLE CHD ---\n")
print(mle_chd$par)
