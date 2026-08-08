install.packages("ISLR")   
library(ISLR)


head(Auto)

auto <- na.omit(Auto)



str(auto)
head(auto)


summary(auto$mpg)
summary(auto$weight)


cat("Media MPG:", round(mean(auto$mpg),4))
cat("Media Weight:", round(mean(auto$weight),4))
cat("Correlación:",round(cor(auto$mpg, auto$weight),4))



plot(auto$weight,auto$mpg,
     main = "Consumo de combustible vs Peso vehicular",
     xlab ="Peso del vehículo(lbs)",
     ylab = "Consumo(MPG)",
     col = "steelblue",
     pch =16)


# --- Modelo clásico ---

modelo_mpg_weight <- lm(mpg ~ weight,data = auto)

summary(modelo_mpg_weight)

# IC clásico
confint(modelo_mpg_weight)


plot(auto$weight,auto$mpg,
     main = "Consumo de combustible vs Peso vehicular",
     xlab ="Peso del vehículo(lbs)",
     ylab = "Consumo(MPG)",
     col = "steelblue",
     pch =16)

abline(modelo_mpg_weight, col = "red", lwd = 2)





#Transformación logaritmica 

plot(log(auto$weight), auto$mpg,
     xlab="log(Peso del vehículo) (lbs)", ylab="Consumo MPG",
     col = "green",
     pch = 16, 
     main="MPG vs log(Peso del vehículo)")
abline(lm(mpg ~ log(weight), data=auto), col="blue", lwd=2)


plot(auto$weight, log(auto$mpg),
     xlab="Peso del vehículo (lbs)", ylab="log(Consumo MPG)",
     col = "orange",
     pch = 16, 
     main="log(MPG) vs Peso del vehículo")
abline(lm(log(mpg) ~ weight, data=auto), col="blue", lwd=2)



#Pruebas aplicadas 

modelo_log <- lm(log(mpg) ~ weight, data = auto)

par(mfrow = c(2,2),
    mar = c(4,4,2,1))

plot(modelo_log,
     col = "steelblue",
     pch = 16)

par(mfrow = c(1,1))



shapiro.test(residuals(modelo_log))

install.packages("lmtest")
library(lmtest)
bptest(modelo_log)



modelo_log1<- lm(mpg ~ log(weight), data = auto)

shapiro.test(residuals(modelo_log1))

library(lmtest)
bptest(modelo_log1)



par(mfrow = c(2,2), mar = c(4,4,2,1))

plot(modelo_log1,
     pch = 16,
     col = "sandybrown",
     col.smooth = "darkblue")

par(mfrow = c(1,1))




