
# BOOTSTRAP - CASO 1
# Transformación en la variable respuesta: log(mpg) ~ weight
# ===========================================================

auto<-read.csv("auto-mpg.csv")

#Modelo clásico 
modelo_log <- lm(log(mpg) ~ weight, data = auto)

summary(modelo_log)

# =========================================================
# ERRORES ESTÁNDAR CLÁSICOS

cat("Error estándar clásico β0:", round(summary(modelo_log)$coefficients[1,2],6), "\n")

cat("Error estándar clásico β1:", round(summary(modelo_log)$coefficients[2,2],6), "\n")

# --- IC clásico ---

confint(modelo_log)

# =========================================================
# BOOTSTRAP POR PARES


set.seed(123)

B <- 1000

beta0_boot1 <- c()
beta1_boot1 <- c()

n <- nrow(auto)

for(i in 1:B){
  
  indices <- sample(1:n, n, replace = TRUE)
  
  muestra_boot <- auto[indices, ]
  
  modelo_boot <- lm(log(mpg) ~ weight, data = muestra_boot)
  
  beta0_boot1[i] <- coef(modelo_boot)[1]
  beta1_boot1[i] <- coef(modelo_boot)[2]
}

# =========================================================
# ERRORES ESTÁNDAR BOOTSTRAP


cat("Error estándar Bootstrap β0:", round(sd(beta0_boot1),6), "\n")

cat("Error estándar Bootstrap β1:", round(sd(beta1_boot1),6), "\n")

# =========================================================
# INTERVALOS DE CONFIANZA BOOTSTRAP


ic_beta0_boot1 <- quantile(beta0_boot1, c(0.025,0.975))

ic_beta1_boot1 <- quantile(beta1_boot1, c(0.025,0.975))

cat("IC Bootstrap β0:\n")
print(round(ic_beta0_boot1,6))

cat("IC Bootstrap β1:\n")
print(round(ic_beta1_boot1,6))

# =========================================================
# COMPARACIÓN


cat("β1 clásico:", round(coef(modelo_log)[2],6), "\n")

cat("β1 Bootstrap:", round(mean(beta1_boot1),6), "\n")



# =========================================================
# HISTOGRAMA BOOTSTRAP β1


hist(beta1_boot1,
     probability = TRUE,
     breaks = 30,
     col = "lightblue",
     border = "white",
     main = expression("Distribución Bootstrap de " ~ beta[1]),
     xlab = expression(beta[1]))

lines(density(beta1_boot1),lwd = 2)

abline(v = coef(modelo_log)[2], col = "red", lwd = 2)

abline(v = ic_beta1_boot1, col = "blue", lty = 2, lwd = 2)



# =========================================================
# IC clásico vs Bootstrap


ic_clasico1 <- confint(modelo_log)[2, ]

plot(c(1,2), c(coef(modelo_log)[2], mean(beta1_boot1)),
     ylim = range(c(ic_clasico1,
                    ic_beta1_boot1)),
     
     pch = 16,
     xaxt = "n",
     xlab = "",
     ylab = expression(beta[1]),
     
     main = "IC clásico vs IC Bootstrap")

axis(1,at = c(1,2), labels = c("Clásico", "Bootstrap"))

arrows(1, ic_clasico1[1], 1, ic_clasico1[2], angle = 90, code = 3, 
       length = 0.08, lwd = 3 )

arrows(2, ic_beta1_boot1[1], 2, ic_beta1_boot1[2], angle = 90, code = 3, 
       length = 0.08, lwd = 3)





# =========================================================
# BOOTSTRAP - CASO 2
# Transformación en la variable predictora: mpg ~ log(weight)



# --- Modelo clásico ---
modelo_log1 <- lm(mpg ~ log(weight), data = auto)

summary(modelo_log1)

# =========================================================
# ERRORES ESTÁNDAR CLÁSICOS

cat("Error estándar clásico β0:", round(summary(modelo_log1)$coefficients[1,2],6), "\n")

cat("Error estándar clásico β1:", round(summary(modelo_log1)$coefficients[2,2],6), "\n")

# --- IC clásico ---
confint(modelo_log1)

# =========================================================
# BOOTSTRAP POR PARES


set.seed(123)

B <- 1000

beta0_boot2 <- c()
beta1_boot2 <- c()

n <- nrow(auto)

for(i in 1:B){
  
  indices <- sample(1:n, n, replace = TRUE)
  
  muestra_boot2 <- auto[indices, ]
  
  modelo_boot2 <- lm(mpg ~ log(weight), data = muestra_boot2)
  
  beta0_boot2[i] <- coef(modelo_boot2)[1]
  beta1_boot2[i] <- coef(modelo_boot2)[2]
}

# =========================================================
# ERRORES ESTÁNDAR BOOTSTRAP


cat("Error estándar Bootstrap β0:", round(sd(beta0_boot2),6), "\n")

cat("Error estándar Bootstrap β1:", round(sd(beta1_boot2),6), "\n")

# =========================================================
# INTERVALOS DE CONFIANZA BOOTSTRAP


ic_beta0_boot2 <- quantile(beta0_boot2, c(0.025,0.975))

ic_beta1_boot2 <- quantile(beta1_boot2, c(0.025,0.975))

cat("IC Bootstrap β0:\n") 
print(round(ic_beta0_boot2,6))

cat("IC Bootstrap β1:\n")
print(round(ic_beta1_boot2,6))

# =========================================================
# COMPARACIÓN


cat("β1 clásico:", round(coef(modelo_log1)[2],6), "\n")

cat("β1 Bootstrap:", round(mean(beta1_boot2),6), "\n")

# =========================================================
# HISTOGRAMA BOOTSTRAP β1


hist(beta1_boot2,
     probability = TRUE,
     breaks = 30,
     col = "orange",
     border = "white",
     main = expression("Distribución Bootstrap de " ~ beta[1]),
     xlab = expression(beta[1]))

lines(density(beta1_boot2), lwd = 2)

abline(v = coef(modelo_log1)[2], col = "red", lwd = 2)

abline(v = ic_beta1_boot2, col = "blue", lty = 2, lwd = 2)


# =========================================================
# IC clásico vs Bootstrap


ic_clasico2 <- confint(modelo_log1)[2, ]

plot(c(1,2), c(coef(modelo_log1)[2], mean(beta1_boot2)),
     ylim = range(c(ic_clasico2, ic_beta1_boot2)),
     
     pch = 16,
     xaxt = "n",
     xlab = "",
     ylab = expression(beta[1]),
     
     main = "IC clásico vs IC Bootstrap")

axis(1, at = c(1,2), labels = c("Clásico", "Bootstrap"))

arrows(1, ic_clasico2[1], 1,ic_clasico2[2], angle = 90, code = 3, 
       length = 0.08, lwd = 3)

arrows(2, ic_beta1_boot2[1], 2, ic_beta1_boot2[2], angle = 90, code = 3,
       length = 0.08, lwd = 3)







# =========================================================
# PREDICCIÓN - PARTICIÓN 70/30 (CASO 1 y CASO 2)


set.seed(123)
n <- nrow(auto)

# Partición: 70% entrenamiento, 30% prueba
train_idx <- sample(1:n, size = round(0.7 * n))
train <- auto[train_idx, ]
test  <- auto[-train_idx, ]

cat("Observaciones entrenamiento:", nrow(train), "\n")
cat("Observaciones prueba:", nrow(test), "\n")

# ---------------------------------------------------------
# CASO 1: log(mpg) ~ weight
# ---------------------------------------------------------
modelo_log_train <- lm(log(mpg) ~ weight, data = train)

# Predicción en escala log, luego se regresa a la escala original (mpg)
pred_log1 <- predict(modelo_log_train, newdata = test)
pred_mpg1 <- exp(pred_log1)   


rmse1 <- sqrt(mean((test$mpg - pred_mpg1)^2))
mae1  <- mean(abs(test$mpg - pred_mpg1))
r2_test1 <- cor(test$mpg, pred_mpg1)^2

cat("Caso 1 -- RMSE:", round(rmse1,3), " MAE:", round(mae1,3),
    " R² (test):", round(r2_test1,4), "\n")

# ---------------------------------------------------------
# CASO 2: mpg ~ log(weight)
# ---------------------------------------------------------
modelo_log1_train <- lm(mpg ~ log(weight), data = train)

pred_mpg2 <- predict(modelo_log1_train, newdata = test)

rmse2 <- sqrt(mean((test$mpg - pred_mpg2)^2))
mae2  <- mean(abs(test$mpg - pred_mpg2))
r2_test2 <- cor(test$mpg, pred_mpg2)^2

cat("Caso 2 -- RMSE:", round(rmse2,3), " MAE:", round(mae2,3),
    " R² (test):", round(r2_test2,4), "\n")

# ---------------------------------------------------------
# GRÁFICO: Predicho vs Observado (ambos casos)
# ---------------------------------------------------------
par(mfrow = c(1,2))

plot(test$mpg, pred_mpg1,
     xlab = "MPG observado", ylab = "MPG predicho",
     main = "Caso 1: Predicho vs Observado",
     col = "steelblue", pch = 16)
abline(0, 1, col = "red", lwd = 2)

plot(test$mpg, pred_mpg2,
     xlab = "MPG observado", ylab = "MPG predicho",
     main = "Caso 2: Predicho vs Observado",
     col = "orange", pch = 16)
abline(0, 1, col = "red", lwd = 2)

par(mfrow = c(1,1))


mean(auto$mpg)

