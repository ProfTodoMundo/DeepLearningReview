

# =========================================================
# GRÁFICA DE CONVERGENCIA DEL GRADIENTE (PÉRDIDA)
# =========================================================
plot(loss_history, type = "l", lwd = 2, col = "blue",
     xlab = "Iteraciones", ylab = "Función de Pérdida (Log-verosimilitud negativa)",
     main = "Convergencia de la Optimización por Gradiente")
grid()

