
# SIMULACIÓN BOOTSTRAP EN REGRESIÓN LINEAL SIMPLE
# Modelo: Y = 3 + 5X + error
# ================================================


set.seed(123)

n <- 50
x <- runif(n, min = 2, max = 30)
y <- 3 + 5*x + rnorm(n, mean = 0, sd = 5)


data <- data.frame(x, y)
View(data)


# --- Modelo clásico ---
ajuste <- lm(y ~ x, data = data)

# --- errores estandar ---
summary(ajuste)

# --- IC clásico ---

confint(ajuste)


plot(x, y, main = "Dispersión de los datos simulados",
     xlab = "variable X",
     ylab ="variable Y",
     col ="steelblue", pch =16)
abline(ajuste, col ="red", lwd =2)



# --- Bootstrap por pares ---
beta0 <- c()
beta1 <- c()
B <- 1000

for(i in 1:B){
  indices     <- sample(1:n, n, replace = TRUE)
  muestraBOOT <- data[indices, ]
  ajusteBOOT  <- lm(y ~ x, data = muestraBOOT)
  beta0[i]    <- ajusteBOOT$coefficients[1]
  beta1[i]    <- ajusteBOOT$coefficients[2]
}



# --- Distribuciones Bootstrap ---
plot(density(beta0), main = "Distribución Bootstrap de β₀")
plot(density(beta1), main = "Distribución Bootstrap de β₁")

# --- Promedios ---
beta0BOOT <- mean(beta0)
beta1BOOT <- mean(beta1)

# --- errores estandar Bootstrap ---
cat("Error estándar Bootstrap β₀:", round(sd(beta0), 4), "\n")
cat("Error estándar Bootstrap β₁:", round(sd(beta1), 4), "\n")


# --- Intervalos de confianza Bootstrap version percentil ---
ic_beta0 <- quantile(beta0, c(0.025, 0.975))
ic_beta1 <- quantile(beta1, c(0.025, 0.975))



#Intervalo de confianza normal Bootstrap
ic_normal0 <- mean(beta0) + c(-1, 1) * 1.96 * sd(beta0)
ic_normal1 <- mean(beta1) + c(-1, 1) * 1.96 * sd(beta1)


cat("IC Bootstrap percentil:", round(ic_beta0, 4), "\n")
cat("IC Bootstrap percentil:", round(ic_beta1, 4), "\n")

cat("IC Bootstrap normal:   ", round(ic_normal0, 4), "\n")
cat("IC Bootstrap normal:   ", round(ic_normal1, 4), "\n")


cat("β₀ verdadero: 3.0000\n")
cat("β₀ clásico: ", round(coef(ajuste)[1], 4), "\n")
cat("β₀ Bootstrap: ", round(beta0BOOT, 4), "\n\n")

cat("β₁ verdadero: 5.0000\n")
cat("β₁ clásico: ", round(coef(ajuste)[2], 4), "\n")
cat("β₁ Bootstrap:  ", round(beta1BOOT, 4), "\n\n")







plot(c(1,2),
     c(coef(ajuste)[2], beta1BOOT),
     xlim = c(0.5, 2.5),
     ylim = range(confint(ajuste)[2,], ic_beta1),
     xaxt = "n",
     pch = 19,
     xlab = "",
     ylab = expression(beta[1]),
     main = "IC clásico vs IC Bootstrap percentil")

axis(1, at = c(1,2),
     labels = c("Clásico", "Bootstrap"))

arrows(1,
       confint(ajuste)[2,1],
       1,
       confint(ajuste)[2,2],
       angle = 90,
       code = 3,
       length = 0.08,
       lwd = 2)

arrows(2,
       ic_beta1[1],
       2,
       ic_beta1[2],
       angle = 90,
       code = 3,
       length = 0.08,
       lwd = 2)

abline(h = 5, col = "red", lty = 2)





hist(beta0,
     probability = TRUE,
     main = expression("Distribución Bootstrap de " ~ beta[0]),
     xlab = expression(beta[0]))

lines(density(beta0), lwd = 2)

abline(v = coef(ajuste)[1], col = "red", lwd = 2)
abline(v = ic_beta0, col = "blue", lwd = 2, lty = 2)



hist(beta1,
     probability = TRUE,
     main = expression("Distribución Bootstrap de " ~ beta[1]),
     xlab = expression(beta[1]))

lines(density(beta1), lwd = 2)

abline(v = coef(ajuste)[2], col = "red", lwd = 2)
abline(v = ic_beta1, col = "blue", lwd = 2, lty = 2)






# SIMULACIÓN BOOTSTRAP EN REGRESIÓN LINEAL SIMPLE
# Escenario 2: Heterocedasticidad
# Modelo: Y = 3 + 5X + error (varianza no constante)
# ================================================

set.seed(123)


n <- 50
x2 <- runif(n, min = 2, max = 30)
y2 <- 3 + 5*x2 + rnorm(n, mean = 0, sd = 0.8*x2)
# la varianza crece con X: sd = 0.5*x

data2 <- data.frame(x2, y2)

# --- Modelo clásico ---
ajuste2 <- lm(y2 ~ x2, data = data2)

# --- Errores estándar ---
summary(ajuste2)

# --- IC clásico ---
confint(ajuste2)



plot(x2, y2,main = "Datos simulados con heterocedasticidad",
     xlab = "X",
     ylab = "Y",
     pch = 16,
     col = "steelblue")

abline(ajuste2,col = "red",lwd = 2)


# --- Bootstrap por pares ---
beta0_2 <- c()
beta1_2 <- c()
B <- 1000

for(i in 1:B){
  indices      <- sample(1:n, n, replace = TRUE)
  muestraBOOT2 <- data2[indices, ]
  ajusteBOOT2  <- lm(y2 ~ x2, data = muestraBOOT2)
  beta0_2[i]   <- ajusteBOOT2$coefficients[1]
  beta1_2[i]   <- ajusteBOOT2$coefficients[2]
}

# --- Distribuciones Bootstrap ---
plot(density(beta0_2), main = "Distribución Bootstrap de β₀")
plot(density(beta1_2), main = "Distribución Bootstrap de β₁")

# --- Promedios ---
beta0BOOT2 <- mean(beta0_2)
beta1BOOT2 <- mean(beta1_2)

# --- Errores estándar Bootstrap ---
cat("Error estándar Bootstrap β₀:", round(sd(beta0_2), 4), "\n")
cat("Error estándar Bootstrap β₁:", round(sd(beta1_2), 4), "\n")

# --- Intervalos de confianza Bootstrap versión percentil ---
ic_beta0_2 <- quantile(beta0_2, c(0.025, 0.975))
ic_beta1_2 <- quantile(beta1_2, c(0.025, 0.975))

# --- intervalo de confianza normal Bootstrap ---
ic_normal0_2 <- mean(beta0_2) + c(-1, 1) * 1.96 * sd(beta0_2)
ic_normal1_2 <- mean(beta1_2) + c(-1, 1) * 1.96 * sd(beta1_2)


cat("IC Bootstrap percentil β₀:", round(ic_beta0_2, 4), "\n")
cat("IC Bootstrap percentil β₁:", round(ic_beta1_2, 4), "\n")
cat("IC Bootstrap normal    β₀:", round(ic_normal0_2, 4), "\n")
cat("IC Bootstrap normal    β₁:", round(ic_normal1_2, 4), "\n")


cat("β₀ verdadero:  3.0000\n")
cat("β₀ clásico: ", round(coef(ajuste2)[1], 4), "\n")
cat("β₀ Bootstrap: ", round(beta0BOOT2, 4), "\n\n")

cat("β₁ verdadero: 5.0000\n")
cat("β₁ clásico:", round(coef(ajuste2)[2], 4), "\n")
cat("β₁ Bootstrap:", round(beta1BOOT2, 4), "\n\n")



hist(beta1_2,
     breaks = 30,
     probability = TRUE,
     col = "#F4A261",
     border = "white",
     main = expression(paste("Distribución Bootstrap de ", beta[1])),
     xlab = expression(hat(beta)[1]))

lines(density(beta1_2),
      lwd = 2)

abline(v = coef(ajuste2)[2],
       col = "red",
       lwd = 3)

abline(v = ic_beta1_2[1],
       col = "blue",
       lty = 2,
       lwd = 2)

abline(v = ic_beta1_2[2],
       col = "blue",
       lty = 2,
       lwd = 2)



ic_clasico <- confint(ajuste2)[2, ]

plot(c(1,2),
     c(coef(ajuste2)[2],
       mean(beta1_2)),
     ylim = range(c(ic_clasico,
                    ic_beta1_2)),
     pch = 16,
     xaxt = "n",
     xlab = "",
     ylab = expression(beta[1]),
     main = "IC clásico vs IC Bootstrap")

axis(1,
     at = c(1,2),
     labels = c("Clásico",
                "Bootstrap"))

arrows(1,
       ic_clasico[1],
       1,
       ic_clasico[2],
       angle = 90,
       code = 3,
       length = 0.08)

arrows(2,
       ic_beta1_2[1],
       2,
       ic_beta1_2[2],
       angle = 90,
       code = 3,
       length = 0.08)

abline(h = 5,
       col = "red",
       lty = 2)




