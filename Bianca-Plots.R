rm(list=ls())
h=function(x,k)
  (1/(2*pi*besselI(k,0)))*exp(k*cos(x))

curve(h(x,0.5),-pi,pi,ylim=c(0,0.8),xlab=expression(paste(theta)),
      ylab=expression(f[VM]*(paste(theta))),lty=1)
curve(h(x,1),lty=2,add=T)
curve(h(x,2),add=T,lty=3)
curve(h(x,3),add=T,lty=4)

legend("topleft",legend=expression(paste(kappa,"=",0.5),
                                   paste(kappa,"=",1),paste(kappa,"=",2),paste(kappa,"=",3)),lty=c(1,2,3,4))

#-------------------------
rm(list=ls())
#library(NPCirc)
library(circular)

par(mar = c(0.3,0.3, 0.3, 0.3))

#von Mises plots
plot.function.circular(function(x) dvonmises(x, circular(0), 0.5), xlim=c(-1, 2.2),ylim=c(-1.5,1.5),lty=2,lwd=1.5, axes = FALSE, tol = 0)
plot.function.circular(function(x) dvonmises(x, circular(0), 0), xlim=c(-1, 2.2),add=T,lty=1,lwd=1.5)
plot.function.circular(function(x) dvonmises(x, circular(0), 1), xlim=c(-1, 2.2),add=T,lty=3,lwd=1.5)
plot.function.circular(function(x) dvonmises(x, circular(0), 2), xlim=c(-1, 2.2),add=T,lty=4,lwd=1.5)
plot.function.circular(function(x) dvonmises(x, circular(0), 3), xlim=c(-1, 2.2),add=T,lty=5,lwd=1.5)

axis.circular(at = NULL, labels = NULL, units = "radians", template = "none", zero = 0, cex = 0.8, tcl.text = 0.2)

legend("topright",legend=expression(paste(kappa,"=",0), paste(kappa,"=",0.5),
                                    paste(kappa,"=",1),paste(kappa,"=",2),paste(kappa,"=",3)),lty=1:5,cex=1, text.width = 1)

#Cardioid Plots
plot.function.circular(function(x) dcardioid(x, circular(0), 0), xlim=c(-1, 2.2),ylim=c(-1.5,1.5),lty=1,lwd=1.5, axes = FALSE, tol = 0)
plot.function.circular(function(x) dcardioid(x, circular(0), -0.2), xlim=c(-1, 2.2),add=T,lty=2,lwd=1.5)
plot.function.circular(function(x) dcardioid(x, circular(0), 0.2), xlim=c(-1, 2.2),add=T,lty=3,lwd=1.5)
plot.function.circular(function(x) dcardioid(x, circular(0), 0.4), xlim=c(-1, 2.2),add=T,lty=4,lwd=1.5)

axis.circular(at = NULL, labels = NULL, units = "radians", template = "none", zero = 0, cex = 0.8, tcl.text = 0.2)

legend("topright",legend=expression(paste(rho,"=",0),
                                    paste(rho,"=",0.2),paste(rho,"=",0.4)),lty=c(1,2,4),cex=1, text.width = 1)

#Uniform Plot
par(mar = c(0.3,7.3,0.3,0.3))
plot.function.circular(function(x) dcircularuniform(x), xlim=c(-1, 2.2),ylim=c(-1.5,1.5),lty=1,lwd=1.5, axes = FALSE, tol = 0)

axis.circular(at = NULL, labels = NULL, units = "radians", template = "none", zero = 0, cex = 0.8, tcl.text = 0.2)

#Wrapped Cauchy Plots
plot.function.circular(function(x) dwrappedcauchy(x, circular(0), 0), xlim=c(-1, 2.2),ylim=c(-1.5,1.5),lty=1,lwd=1.5, axes = FALSE, tol = 0)
plot.function.circular(function(x) dwrappedcauchy(x, circular(0), 0.2), xlim=c(-1, 2.2),add=T,lty=2,lwd=1.5)
plot.function.circular(function(x) dwrappedcauchy(x, circular(0), 0.5), xlim=c(-1, 2.2),add=T,lty=3,lwd=1.5)
plot.function.circular(function(x) dwrappedcauchy(x, circular(0), 0.8), xlim=c(-1, 2.2),add=T,lty=4,lwd=1.5)

axis.circular(at = NULL, labels = NULL, units = "radians", template = "none", zero = 0, cex = 0.8, tcl.text = 0.2)

legend("topright",legend=expression(paste(rho,"=",0),
                                    paste(rho,"=",0.2),paste(rho,"=",0.5), paste(rho,"=",0.8)),lty=1:4,cex=1, text.width = 1)


