# Exploración empirica del teorema BICKEL & FREEDMAN (1981)
# Bootstrap por pares (con reemplazo)
# Verificación empírica cuando los supuestos se cumplen (cuando se hace simulaciones)
# ====================================================================================

set.seed(123)


beta0 <- 3
beta1 <- 5
sigma <- 5


N_pob <- 10000
R     <- 200      # Réplicas por n
B     <- 500      # Réplicas bootstrap
tamanos <- c(30, 50, 80, 120, 200, 300, 500)


# --- CREAR POBLACIÓN ---
x_pob <- runif(N_pob, 2, 30)
y_pob <- beta0 + beta1 * x_pob + rnorm(N_pob, 0, sigma)
pob <- data.frame(x = x_pob, y = y_pob)

# Varianza teórica de X (constante)
var_x_teorica <- var(x_pob)


cat("POBLACIÓN SIMULADA\n")

cat("Tamaño:", N_pob, "observaciones\n")
cat("Var(X) =", round(var_x_teorica, 4), "\n")
cat("β₁ =", beta1, "  σ =", sigma, "\n\n")


ks_promedio <- c()
ks_mediana <- c()
ks_min <- c()
ks_max <- c()
ks_todos <- list()


for (n in tamanos) {
  
  cat("Tamaño de muestra: n =", n, "\n")
  
  ks_replicas <- numeric(R)
  pb <- txtProgressBar(min = 0, max = R, style = 3)
  
  for (r in 1:R) {
    
    
    idx_muestra <- sample(1:N_pob, size = n, replace = FALSE)
    mi_muestra <- pob[idx_muestra, ]
    
    modelo <- lm(y ~ x, data = mi_muestra)
    beta_hat <- coef(modelo)[2]
    sigma2_hat <- summary(modelo)$sigma^2
    
    var_asintotica <- sigma2_hat / var_x_teorica
    
    T_boot <- numeric(B)
    
    for (b in 1:B) {
      
      idx_boot <- sample(1:n, size = n, replace = TRUE)
      boot_data <- mi_muestra[idx_boot, ]
      
      modelo_boot <- lm(y ~ x, data = boot_data)
      beta_star <- coef(modelo_boot)[2]
      
      
      T_boot[b] <- sqrt(n) * (beta_star - beta_hat)
    }
    
    
    ks_test <- suppressWarnings(
      ks.test(T_boot, "pnorm", mean = 0, sd = sqrt(var_asintotica))
    )
    
    ks_replicas[r] <- ks_test$statistic
    setTxtProgressBar(pb, r)
  }
  
  close(pb)
  
  ks_promedio <- c(ks_promedio, mean(ks_replicas))
  ks_mediana <- c(ks_mediana, median(ks_replicas))
  ks_min <- c(ks_min, min(ks_replicas))
  ks_max <- c(ks_max, max(ks_replicas))
  ks_todos[[length(ks_todos) + 1]] <- ks_replicas
  
  cat("  KS promedio:", round(mean(ks_replicas), 4), "\n")
  cat("  KS mediana :", round(median(ks_replicas), 4), "\n")
  cat("  KS (min-max):", round(min(ks_replicas), 4), "-", round(max(ks_replicas), 4), "\n")
}



# --- TABLA DE RESULTADOS ---
tabla_final <- data.frame(
  n = tamanos,
  KS_promedio = round(ks_promedio, 4),
  KS_mediana = round(ks_mediana, 4),
  Reduccion = c(NA, round(100 * (ks_promedio[1] - ks_promedio[-1]) / ks_promedio[1], 1))
)


cat("TABLA DE RESULTADOS - BOOTSTRAP POR PARES\n")
print(tabla_final, row.names = FALSE)


par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))


plot(tamanos, ks_promedio,
     type = "b", pch = 16, col = "darkred", lwd = 2.5, cex = 1.5,
     xlab = "Tamaño de muestra (n)",
     ylab = "Distancia KS promedio",
     main = "Bootstrap por pares: KS vs normal asintótica",
     ylim = c(0, max(ks_promedio) + 0.02),
     cex.main = 1.2, cex.lab = 1.2)

arrows(tamanos, ks_min, tamanos, ks_max,
       length = 0.05, angle = 90, code = 3, col = "gray50", lwd = 1.5)


lines(lowess(tamanos, ks_promedio), col = "darkblue", lwd = 2.5, lty = 4)

abline(h = 0, col = "gray", lty = 2, lwd = 2)


text(tamanos, ks_promedio + 0.005,
     labels = round(ks_promedio, 4), cex = 0.9, font = 2)

legend("topright",
       legend = c("KS promedio", "Rango (min-max)", "Tendencia"),
       col = c("darkred", "gray50", "darkblue"),
       lty = c(1, 1, 4), lwd = c(2.5, 1.5, 2.5),
       pch = c(16, NA, NA), cex = 0.8, bty = "n")

# Gráfico 2: Boxplots
boxplot(ks_todos, names = tamanos,
        col = "lightblue", border = "darkblue",
        main = "Distribución de KS por n (bootstrap por pares)",
        xlab = "Tamaño de muestra (n)",
        ylab = "Distancia KS",
        cex.main = 1.2, cex.lab = 1.2,
        ylim = c(0, max(unlist(ks_todos)) + 0.01))
abline(h = 0, col = "gray", lty = 2, lwd = 2)

par(mfrow = c(1, 1))



cat("INTERPRETACIÓN - BOOTSTRAP POR PARES\n")


primer_ks <- ks_promedio[1]
ultimo_ks <- tail(ks_promedio, 1)
reduccion_total <- round(100 * (primer_ks - ultimo_ks) / primer_ks, 1)

cat("\n Resultados con bootstrap por pares:\n")
cat("\n   • KS promedio:", round(primer_ks, 4), "(n =", tamanos[1], ")")
cat("\n                →", round(ultimo_ks, 4), "(n =", tail(tamanos, 1), ")")
cat("\n   • Reducción:", reduccion_total, "%\n")







# ESCENARIO 2: HETEROCEDASTICIDAD
# Verificación empírica cuando los supuestos NO se cumplen (cuando se hace simulaciones)
# =======================================================================================
# 
# Modelo: Y = 3 + 5X + ε, donde ε ~ N(0, 0.5*X)
# VARIANZA ASINTÓTICA UTILIZADA:
# Var(√n(β̂ - β)) = Q⁻¹ Ω Q⁻¹
# donde Q = E[XᵢXᵢᵀ] y Ω = E[XᵢXᵢᵀ εᵢ²]
# =========================================================

set.seed(123)


beta0 <- 3
beta1 <- 5


N_pob <- 10000
R     <- 200      # Réplicas por n
B     <- 500      # Réplicas bootstrap
tamanos <- c(30, 50, 80, 120, 200, 300, 500)


# --- CREAR POBLACIÓN CON HETEROCEDASTICIDAD ---
x_pob <- runif(N_pob, 2, 30)
y_pob <- beta0 + beta1 * x_pob + rnorm(N_pob, mean = 0, sd = 0.5 * x_pob)
pob <- data.frame(x = x_pob, y = y_pob)



ks_promedio <- c()
ks_mediana <- c()
ks_min <- c()
ks_max <- c()
ks_todos <- list()


for (n in tamanos) {
  
  cat("Tamaño de muestra: n =", n, "\n")
  
  
  ks_replicas <- numeric(R)
  pb <- txtProgressBar(min = 0, max = R, style = 3)
  
  for (r in 1:R) {
    
    
    idx_muestra <- sample(1:N_pob, size = n, replace = FALSE)
    mi_muestra <- pob[idx_muestra, ]
    
    
    modelo <- lm(y ~ x, data = mi_muestra)
    beta_hat <- coef(modelo)[2]
    residuos <- residuals(modelo)
    
    
    X_mat <- cbind(1, mi_muestra$x)  
    
    
    Q <- t(X_mat) %*% X_mat / n
    
    
    Omega <- t(X_mat) %*% diag(residuos^2) %*% X_mat / n
    
    
    Q_inv <- solve(Q)
    sandwich <- Q_inv %*% Omega %*% Q_inv
    
    
    var_asintotica <- sandwich[2, 2]
    
    
    T_boot <- numeric(B)
    
    for (b in 1:B) {
      
      idx_boot <- sample(1:n, size = n, replace = TRUE)
      boot_data <- mi_muestra[idx_boot, ]
      
      modelo_boot <- lm(y ~ x, data = boot_data)
      beta_star <- coef(modelo_boot)[2]
      
      
      T_boot[b] <- sqrt(n) * (beta_star - beta_hat)
    }
    
    
    ks_test <- suppressWarnings(
      ks.test(T_boot, "pnorm", mean = 0, sd = sqrt(var_asintotica))
    )
    
    ks_replicas[r] <- ks_test$statistic
    setTxtProgressBar(pb, r)
  }
  
  close(pb)
  
  
  ks_promedio <- c(ks_promedio, mean(ks_replicas))
  ks_mediana <- c(ks_mediana, median(ks_replicas))
  ks_min <- c(ks_min, min(ks_replicas))
  ks_max <- c(ks_max, max(ks_replicas))
  ks_todos[[length(ks_todos) + 1]] <- ks_replicas
  
  cat("  KS promedio:", round(mean(ks_replicas), 4), "\n")
  cat("  KS mediana :", round(median(ks_replicas), 4), "\n")
  cat("  KS (min-max):", round(min(ks_replicas), 4), "-", round(max(ks_replicas), 4), "\n")
}


tabla_final_hetero <- data.frame(
  n = tamanos,
  KS_promedio = round(ks_promedio, 4),
  KS_mediana = round(ks_mediana, 4),
  Reduccion = c(NA, round(100 * (ks_promedio[1] - ks_promedio[-1]) / ks_promedio[1], 1))
)


cat("TABLA DE RESULTADOS - ESCENARIO 2 (HETEROCEDASTICIDAD)\n")
print(tabla_final_hetero, row.names = FALSE)




par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))


plot(tamanos, ks_promedio,
     type = "b", pch = 16, col = "darkred", lwd = 2.5, cex = 1.5,
     xlab = "Tamaño de muestra (n)",
     ylab = "Distancia KS promedio",
     main = "Escenario 2: Heterocedasticidad",
     ylim = c(0, max(ks_promedio) + 0.02),
     cex.main = 1.2, cex.lab = 1.2)


arrows(tamanos, ks_min, tamanos, ks_max,
       length = 0.05, angle = 90, code = 3, col = "gray50", lwd = 1.5)


lines(lowess(tamanos, ks_promedio), col = "darkblue", lwd = 2.5, lty = 4)


abline(h = 0, col = "gray", lty = 2, lwd = 2)


text(tamanos, ks_promedio + 0.005,
     labels = round(ks_promedio, 4), cex = 0.9, font = 2)

legend("topright",
       legend = c("KS promedio", "Rango (min-max)", "Tendencia"),
       col = c("darkred", "gray50", "darkblue"),
       lty = c(1, 1, 4), lwd = c(2.5, 1.5, 2.5),
       pch = c(16, NA, NA), cex = 0.8, bty = "n")


boxplot(ks_todos, names = tamanos,
        col = "lightcoral", border = "darkred",
        main = "Distribución de KS por n (Heterocedasticidad)",
        xlab = "Tamaño de muestra (n)",
        ylab = "Distancia KS",
        cex.main = 1.2, cex.lab = 1.2,
        ylim = c(0, max(unlist(ks_todos)) + 0.01))
abline(h = 0, col = "gray", lty = 2, lwd = 2)

par(mfrow = c(1, 1))





cat("INTERPRETACIÓN - ESCENARIO 2 (HETEROCEDASTICIDAD)\n")


primer_ks <- ks_promedio[1]
ultimo_ks <- tail(ks_promedio, 1)
reduccion_total <- round(100 * (primer_ks - ultimo_ks) / primer_ks, 1)

cat("\n📌 Resultados con heterocedasticidad (varianza sandwich):\n")
cat("\n   • KS promedio:", round(primer_ks, 4), "(n =", tamanos[1], ")")
cat("\n                →", round(ultimo_ks, 4), "(n =", tail(tamanos, 1), ")")
cat("\n   • Reducción:", reduccion_total, "%\n")





# =========================================================================
# Exploracion empirica del teorema BICKEL & FREEDMAN (1981) - Auto MPG
# =========================================================================
# El dataset de 398 observaciones es la muestra disponible.
# No hay una "población" separada.
# El bootstrap se aplica directamente sobre los datos.
# =========================================================

auto1<-read.csv("auto-mpg.csv")
auto <- na.omit(auto)


set.seed(123)


R <- 200     
B <- 500     
tamanos <- c(30, 50, 80, 120, 160, 200, 250, 300, 350)


N_total <- nrow(auto)
beta1_ref_caso1 <- coef(lm(log(mpg) ~ weight, data = auto))[2]
beta1_ref_caso2 <- coef(lm(mpg ~ log(weight), data = auto))[2]


cat("ENFOQUE: DATOS REALES (sin población separada)\n")

cat("Muestra disponible:", N_total, "observaciones\n")
cat("β₁ de referencia (Caso 1):", round(beta1_ref_caso1, 6), "\n")
cat("β₁ de referencia (Caso 2):", round(beta1_ref_caso2, 6), "\n")
cat("\nNOTA: El bootstrap se aplica directamente sobre los datos.\n")
cat("      No hay una 'población' separada.\n\n")





cat("===================================================\n")
cat("CASO 1: log(mpg) ~ weight\n")
cat("===================================================\n")

ks_promedio_c1 <- c()
ks_mediana_c1 <- c()
ks_min_c1 <- c()
ks_max_c1 <- c()
ks_todos_c1 <- list()

for (n in tamanos) {
  
  cat("\nTamaño de muestra: n =", n, "\n")
  
  ks_replicas <- numeric(R)
  pb <- txtProgressBar(min = 0, max = R, style = 3)
  
  for (r in 1:R) {
    
    
    idx_muestra <- sample(1:N_total, size = n, replace = FALSE)
    mi_muestra <- auto[idx_muestra, ]
    
    
    modelo <- lm(log(mpg) ~ weight, data = mi_muestra)
    beta_hat <- coef(modelo)[2]
    sigma2_hat <- summary(modelo)$sigma^2
    
    
    var_x_muestra <- var(mi_muestra$weight)
    var_asintotica <- sigma2_hat / var_x_muestra
    
    
    T_boot <- numeric(B)
    
    for (b in 1:B) {
      idx_boot <- sample(1:n, size = n, replace = TRUE)
      boot_data <- mi_muestra[idx_boot, ]
      modelo_boot <- lm(log(mpg) ~ weight, data = boot_data)
      beta_star <- coef(modelo_boot)[2]
      T_boot[b] <- sqrt(n) * (beta_star - beta_hat)
    }
    
    
    ks_test <- suppressWarnings(
      ks.test(T_boot, "pnorm", mean = 0, sd = sqrt(var_asintotica))
    )
    
    ks_replicas[r] <- ks_test$statistic
    setTxtProgressBar(pb, r)
  }
  
  close(pb)
  
  ks_promedio_c1 <- c(ks_promedio_c1, mean(ks_replicas))
  ks_mediana_c1 <- c(ks_mediana_c1, median(ks_replicas))
  ks_min_c1 <- c(ks_min_c1, min(ks_replicas))
  ks_max_c1 <- c(ks_max_c1, max(ks_replicas))
  ks_todos_c1[[length(ks_todos_c1) + 1]] <- ks_replicas
  
  cat("  KS promedio:", round(mean(ks_replicas), 4), "\n")
}



cat("\n\n===================================================\n")
cat("CASO 2: mpg ~ log(weight)\n")
cat("===================================================\n")

ks_promedio_c2 <- c()
ks_mediana_c2 <- c()
ks_min_c2 <- c()
ks_max_c2 <- c()
ks_todos_c2 <- list()

for (n in tamanos) {
  
  cat("\nTamaño de muestra: n =", n, "\n")
  
  ks_replicas <- numeric(R)
  pb <- txtProgressBar(min = 0, max = R, style = 3)
  
  for (r in 1:R) {
    
    
    idx_muestra <- sample(1:N_total, size = n, replace = FALSE)
    mi_muestra <- auto[idx_muestra, ]
    
    
    modelo <- lm(mpg ~ log(weight), data = mi_muestra)
    beta_hat <- coef(modelo)[2]
    residuos <- residuals(modelo)
    
    
    X_mat <- cbind(1, log(mi_muestra$weight))
    Q <- t(X_mat) %*% X_mat / n
    Omega <- t(X_mat) %*% diag(residuos^2) %*% X_mat / n
    Q_inv <- solve(Q)
    var_asintotica <- (Q_inv %*% Omega %*% Q_inv)[2, 2]
    
    
    T_boot <- numeric(B)
    
    for (b in 1:B) {
      idx_boot <- sample(1:n, size = n, replace = TRUE)
      boot_data <- mi_muestra[idx_boot, ]
      modelo_boot <- lm(mpg ~ log(weight), data = boot_data)
      beta_star <- coef(modelo_boot)[2]
      T_boot[b] <- sqrt(n) * (beta_star - beta_hat)
    }
    
    
    ks_test <- suppressWarnings(
      ks.test(T_boot, "pnorm", mean = 0, sd = sqrt(var_asintotica))
    )
    
    ks_replicas[r] <- ks_test$statistic
    setTxtProgressBar(pb, r)
  }
  
  close(pb)
  
  ks_promedio_c2 <- c(ks_promedio_c2, mean(ks_replicas))
  ks_mediana_c2 <- c(ks_mediana_c2, median(ks_replicas))
  ks_min_c2 <- c(ks_min_c2, min(ks_replicas))
  ks_max_c2 <- c(ks_max_c2, max(ks_replicas))
  ks_todos_c2[[length(ks_todos_c2) + 1]] <- ks_replicas
  
  cat("  KS promedio:", round(mean(ks_replicas), 4), "\n")
}


# =========================================================
# TABLA COMPARATIVA
# =========================================================

tabla_comparativa <- data.frame(
  n = tamanos,
  KS_C1_prom = round(ks_promedio_c1, 4),
  KS_C1_med = round(ks_mediana_c1, 4),
  KS_C2_prom = round(ks_promedio_c2, 4),
  KS_C2_med = round(ks_mediana_c2, 4)
)


cat("TABLA COMPARATIVA DE RESULTADOS\n")

print(tabla_comparativa, row.names = FALSE)


# =========================================================
# GRÁFICO COMPARATIVO
# =========================================================


par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))


plot(tamanos, ks_promedio_c1,
     type = "b", pch = 16, col = "darkblue", lwd = 2.5, cex = 1.5,
     xlab = "Tamaño de muestra (n)",
     ylab = "Distancia KS promedio",
     main = "Comparación: Caso 1 vs Caso 2",
     ylim = c(0, max(c(ks_promedio_c1, ks_promedio_c2)) + 0.02),
     cex.main = 1.2, cex.lab = 1.2)

lines(tamanos, ks_promedio_c2,
      type = "b", pch = 17, col = "darkred", lwd = 2.5, cex = 1.5)

abline(h = 0, col = "gray", lty = 2, lwd = 2)

legend("topright",
       legend = c("Caso 1: log(mpg) ~ weight", "Caso 2: mpg ~ log(weight)"),
       col = c("darkblue", "darkred"),
       pch = c(16, 17), lwd = 2.5, cex = 0.9, bty = "n")

boxplot(list(Caso1 = unlist(ks_todos_c1), Caso2 = unlist(ks_todos_c2)),
        col = c("lightblue", "lightcoral"),
        main = "Distribución de KS por caso",
        xlab = "Caso de estudio",
        ylab = "Distancia KS",
        cex.main = 1.2, cex.lab = 1.2)
abline(h = 0, col = "gray", lty = 2, lwd = 2)

par(mfrow = c(1, 1))




cat("INTERPRETACIÓN FINAL\n")


cat("\nComparación de casos:\n")
cat("\n   • Caso 1 (log(mpg) ~ weight):")
cat("\n     - KS promedio inicial:", round(ks_promedio_c1[1], 4))
cat("\n     - KS promedio final:", round(tail(ks_promedio_c1, 1), 4))
cat("\n     - Reducción:", round(100*(ks_promedio_c1[1]-tail(ks_promedio_c1,1))/ks_promedio_c1[1], 1), "%")
cat("\n   • Caso 2 (mpg ~ log(weight)):")
cat("\n     - KS promedio inicial:", round(ks_promedio_c2[1], 4))
cat("\n     - KS promedio final:", round(tail(ks_promedio_c2, 1), 4))
cat("\n     - Reducción:", round(100*(ks_promedio_c2[1]-tail(ks_promedio_c2,1))/ks_promedio_c2[1], 1), "%")



