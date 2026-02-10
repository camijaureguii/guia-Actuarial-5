#install.packages("tidyverse")
library(tibble)
bono <- tibble( Plazo = 1:4, FF = c(7, 7, 7, 107))
bono

#Duration
#3 year coupon bond with a face value of 100$, the yield is 12% with continuos compounding.
#Coupon payments are of 5$ every 6 months

Data_Hull_Ed7 <- tibble(
  t = seq(0.5, 3, by = 0.5),
  FD = exp(-t * 0.12),
  CF = rep(5, 6) + c(rep(0, 5), 100),
  PV = FD * CF,
  peso = FD * CF / sum(FD * CF),
  t_x_peso = t * peso)

Data_Hull_Ed7

# Precio del bono
Precio <- sum(Data_Hull_Ed7$PV)
Precio

# Duration de Macaulay
Duration <- sum(Data_Hull_Ed7$t_x_peso)
Duration

# Cambio de precio aproximado ante aumento de tasa 1% 
delta_Precio <- -0.001 * Precio * Duration
delta_Precio

# Precio ajustado
Precio_nuevo <- Precio + delta_Precio
Precio_nuevo

#install.packages("YieldCurve")
library("YieldCurve")
xp <- c(1, 2, 3, 5, 10, 20, 30)
# Tasas spot observadas (ejemplo en %) (de 0 a m años)
fp <- c(5.2, 5.4, 5.6, 6.0, 6.3, 6.5, 6.6)

NS <- Nelson.Siegel(fp, xp)

NSCurve <- function(x){
  b0 = NS[1]
  b1 = NS[2]
  b2 = NS[3]
  l = NS[4]
  Rm = b0 + (b1+b2) * (1 - exp(-x / l)) / (x / l) - b2*exp(-x / l)  
  return(Rm)
}

curve(NSCurve, 0, 30, add = TRUE, col = "blue")

NSCurve(12)

#Guia 1
#ejercicio 1
preciocupon <- c(945, 890, 835, 785)
Yield <- function(precios, VN){
  n <- length(precios)
  Periodo = c(1:4)
  Fdesc = precios/VN
  Tspot = VN/precios - 1
  Tspotanual = (Tspot+1)^(1/Periodo)-1
  tabla = data.frame(Periodo,Precios=precios, FD=Fdesc, TS=Tspot, TSA=Tspotanual)
  Tforward <- numeric(n)
  Tforward[1] <- Tspot[1]               
  if (n > 1) Tforward[2:n] <- Fdesc[1:(n-1)]/Fdesc[2:n] - 1  
  tabla$Tforward <- Tforward
  tasapar= (1-Fdesc[n]) / sum(Fdesc)
  return(list(tabla = tabla, tasa_par = tasapar))
}
Yield(preciocupon,1000)

#ejercicio 2
preciocupon <- c(945, 890, 835, 785)
precio_dur_anu <- function(cup,preciobon,VN,var){
  desc = preciobon/1000
  n <- length(desc)
  precio = sum(cup*VN*desc)+VN*desc[n]
  periodo <- c(1:n)
  dur <- (sum(periodo*cup*VN*desc)+ periodo[n]*VN*desc[n])/precio
  nuevo_precio <- precio*(1-var/100*dur)
  return(paste("el precio es",precio,", la duration es",dur,"y si hay una variación del",var,"% el nuevo precio es",nuevo_precio))
}
precio_dur_anu(0.07,preciocupon,100,1)

#Ejercicio 3
precio_dur_sem <- function(cup,n,VN,var){
  periodo <- seq(0.5,n, by=0.5)
  y <- length(periodo)
  desc <- exp(-0.12*periodo)
  precio = sum(cup/2*VN*desc)+VN*desc[y]
  dur <- (sum(periodo*cup/2*VN*desc)+ periodo[y]*VN*desc[y])/precio
  nuevo_precio_aum <- precio*(1-var/100*dur)
  nuevo_precio_dis <- precio*(1+var/100*dur)
  return(paste("el precio es",precio,", la duration es",dur,", si hay un aumento del",var,"% el nuevo precio es",nuevo_precio_aum,"y si disminuye",nuevo_precio_dis))
}
precio_dur_sem(0.1,3,100,1)

