# Circular -linear regression model
# Fisher, Lee

library(circular)
set.seed(1234)

x <- seq(-1,1, length.out = 100)

g1 <- 2*atan(3*x)
theta1 <- c()
for (i in 1:100) {
  theta1[i] <- rvonmises(1, g1[i], 3)
}

g2 <- 2*atan(3*x)
theta2 <- c()
for (i in 1:100) {
  theta2[i] <- rvonmises(1, g2[i], 12)
}

g3 <- 2*atan(12*x)
theta3 <- c()
for (i in 1:100) {
  theta3[i] <- rvonmises(1, g3[i], 3)
}

g4 <- 2*atan(12*x)
theta4 <- c()
for (i in 1:100) {
  theta4[i] <- rvonmises(1, g4[i], 12)
}

par(mfrow = c(2,2))
plot(x,theta1, ylim=c(0,12), pch = 19, ylab = expression(Theta), main = expression(paste(beta, " = 3, ", kappa, " = 3")))
points(x, (theta1+2*pi), pch = 19)


plot(x,theta2, ylim=c(0,12), pch = 19, ylab = expression(Theta), main = expression(paste(beta, " = 3, ", kappa, " = 12")))
points(x, (theta2+2*pi), pch = 19)

plot(x,theta3, ylim=c(0,12), pch = 19, ylab = expression(Theta), main = expression(paste(beta, " = 12, ", kappa, " = 3")))
points(x, (theta3+2*pi), pch = 19)

plot(x,theta4, ylim=c(0,12), pch = 19, ylab = expression(Theta), main = expression(paste(beta, " = 12, ", kappa, " = 12")))
points(x, (theta4+2*pi), pch = 19)
