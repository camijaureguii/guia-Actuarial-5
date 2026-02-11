#ejercicio 1 ------------------------------------------------------------------------------------------
S0_1 <- 70; K_1 <- 68; r_1 <- 0.05; vol_1 <- 0.2; T_1 <- 0.5; n_1 <- 2 ; dt_1= T_1/n_1
arbol_1 = arbol_subyacente <- function(S0, vol, T, n) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  arbol <- matrix(NA, nrow = n+1, ncol = n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbol[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  colnames(arbol) <- round(seq(0, T, by = dt), 4)
  return(round(arbol, 4))
}

arbol_subyacente(S0_1,vol_1,T_1,n_1)

precio_call_europeo <- function(S0, K, r, vol, T, n) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  p <- (exp(r*dt) - d) / (u - d)   
  arbolS <- matrix(NA, nrow=n+1, ncol=n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbolS[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  arbolF <- matrix(NA, nrow=n+1, ncol=n+1)
  for (j in 0:n) {
    arbolF[j+1, n+1] <- max(0, arbolS[j+1, n+1] - K)
  }
  for (i in (n-1):0) {
    for (j in 0:i) {
      arbolF[j+1, i+1] <- exp(-r*dt) * (p* arbolF[j+1,i+2] + (1-p) * arbolF[j+2,i+2])}}
  return(list(precio=arbolF[1,1], arbolSubyacente=arbolS, arbolDerivado=arbolF))
}
res_1 <- precio_call_europeo(S0_1,K_1,r_1,vol_1,T_1,n_1)
res_1

a_por_nodo_call <- function(S0, K, r, vol, T, n) {
  arbol <- precio_call_europeo(S0, K, r, vol, T, n)
  arbolS <- arbol$arbolSubyacente
  arbolF <- arbol$arbolDerivado
  
  a_nodos <- matrix(NA, nrow=n, ncol=n)
  for (i in (n-1):0) {
    for (j in 0:i) {
      fu <- arbolF[j+1, i+2]   # valor call si sube
      fd <- arbolF[j+2, i+2]   # valor call si baja
      Su <- arbolS[j+1, i+2]   # subyacente si sube
      Sd <- arbolS[j+2, i+2]   # subyacente si baja
      a_nodos[j+1, i+1] <- (fu - fd) / (Su - Sd)}}
  return(a_nodos)
}
a_1 <- a_por_nodo_call(S0_1,K_1,r_1,vol_1,T_1,n_1)
a_1

#si precio es $6.5 el valor del call esta caro:
S0_1 <- 70; K_1 <- 68; r_1 <- 0.05; vol_1 <- 0.2; T_1 <- 0.5; n_1 <- 2 ; dt_1= T_1/n_1
# t= 0: vendo call, compro cartera. La tabla de a me dice de comprar 0.652 acciones, con eso tomo prestamo o invierto.
b0_1 = 6.5 - a_1[1,1]* res_1$arbolSubyacente[1,1]
b0_1
#en t=1 aumenta, por lo que debo llegar a tener 1 de acciones, compro la diferencia de lo que ya tengo
b1_1 = (a_1[1,1] - a_1[1,2])*res_1$arbolSubyacente[1,2]
#en t=2 de nuevo sube
arbi_1 = b0_1*exp(r_1*T_1) + b1_1*exp(r_1*dt_1) - res_1$arbolDerivado[1,3] + a_1[1,2]*res_1$arbolSubyacente[1,3]
arbi_1_en0 = arbi_1 *exp(-r_1*T_1)
arbi_1_en0
#corroboro:
arbi_1_corrob = 6.5-res_1$precio
arbi_1_corrob

#si precio es $5 el valor del call esta barato:
S0_1 <- 70; K_1 <- 68; r_1 <- 0.05; vol_1 <- 0.2; T_1 <- 0.5; n_1 <- 2 ; dt_1= T_1/n_1
# t= 0: compro call, vendo cartera. La tabla de a me dice de vender 0.652 acciones, con eso tomo prestamo o invierto.
b0_1 =  - 5 + a_1[1,1]* res_1$arbolSubyacente[1,1]
b0_1
#en t=1 aumenta, por lo que debo llegar a tener 1 de acciones vendidas, vendo la diferencia de lo que ya tengo
b1_1 = (a_1[1,2] - a_1[1,1])*res_1$arbolSubyacente[1,2]
#en t=2 de nuevo sube
arbi_1 = b0_1*exp(r_1*T_1) + b1_1*exp(r_1*dt_1) + res_1$arbolDerivado[1,3] - a_1[1,2]*res_1$arbolSubyacente[1,3]
arbi_1_en0 = arbi_1 *exp(-r_1*T_1)
arbi_1_en0
#corroboro:
arbi_1_corrob = res_1$precio - 5
arbi_1_corrob

#ejercicio 2 -------------------------------------------------------------------------------------------------
S0_2 <- 75; K_2 <- 78; r_2 <- 0.06; vol_2 <- 0.25; T_2 <- 0.5; n_2 <- 2 ; dt_2= T_2/n_2
arbol_2 = arbol_subyacente(S0_2,vol_2,T_2,n_2)
precio_put_europeo <- function(S0, K, r, vol, T, n) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  p <- (exp(r*dt) - d) / (u - d)   
  arbolS <- matrix(NA, nrow=n+1, ncol=n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbolS[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  arbolF <- matrix(NA, nrow=n+1, ncol=n+1)
  for (j in 0:n) {
    arbolF[j+1, n+1] <- max(0, K - arbolS[j+1, n+1])
  }
  for (i in (n-1):0) {
    for (j in 0:i) {
      arbolF[j+1, i+1] <- exp(-r*dt) * (p* arbolF[j+1,i+2] + (1-p) * arbolF[j+2,i+2])}}
  return(list(precio=arbolF[1,1], arbolSubyacente=arbolS, arbolDerivado=arbolF))
}
res_2 <- precio_put_europeo(S0_2,K_2,r_2,vol_2,T_2,n_2)
res_2 

a_por_nodo_put <- function(S0, K, r, vol, T, n) {
  arbol <- precio_put_europeo(S0, K, r, vol, T, n)
  arbolS <- arbol$arbolSubyacente
  arbolF <- arbol$arbolDerivado
  
  a_nodos <- matrix(NA, nrow=n, ncol=n)
  for (i in (n-1):0) {
    for (j in 0:i) {
      fu <- arbolF[j+1, i+2]   # valor call si sube
      fd <- arbolF[j+2, i+2]   # valor call si baja
      Su <- arbolS[j+1, i+2]   # subyacente si sube
      Sd <- arbolS[j+2, i+2]   # subyacente si baja
      a_nodos[j+1, i+1] <- (fu - fd) / (Su - Sd)}}
  return(a_nodos)
}
a_2 <- a_por_nodo_put(S0_2,K_2,r_2,vol_2,T_2,n_2)
a_2

#si precio es $6 el valor del put esta caro:
# t= 0: vendo put, compro cartera. La tabla de a me dice de vender 0.4925 acciones, con lo recaudado invierto.
b0_2 = 6 - a_2[1,1]* res_2$arbolSubyacente[1,1]
#en t=1 baja, por lo que debo llegar a tener 1 de acciones vendidas, vendo la diferencia de lo que ya tengo
b1_2 = (a_2[1,1] - a_2[2,2])*res_2$arbolSubyacente[2,2]
#en t=2 de nuevo baja
arbi_2 = b0_2*exp(r_2*T_2) + b1_2*exp(r_2*dt_2) - res_2$arbolDerivado[3,3] + a_2[2,2]*res_2$arbolSubyacente[3,3]
arbi_2_en0 = arbi_2 *exp(-r_2*T_2)
arbi_2_en0
#corroboro:
arbi_2_corrob = 6-res_2$precio
arbi_2_corrob

#ejercicio 3 -----------------------------------------------------------------------------------------
S0_3 <- 70; K_3 <- 68; r_3 <- 0.05; vol_3 <- 0.2; T_3 <- 2/12; n_3 <- 2 ; dt_3= T_3/n_3
arbol_3 = arbol_subyacente(S0_3,vol_3,T_3,n_3)
res_3 <- precio_put_europeo(S0_3,K_3,r_3,vol_3,T_3,n_3)
res_3 
a_3 <- a_por_nodo_put(S0_3,K_3,r_3,vol_3,T_3,n_3)
a_3

#si precio es $3 el valor del put esta caro:
# t= 0: vendo put, compro cartera. La tabla de a me dice de vender 0.33 acciones, con lo recaudado invierto.
b0_3 = 3 - a_3[1,1]* res_3$arbolSubyacente[1,1]
#en t=1 baja, por lo que debo llegar a tener 0.738 de acciones vendidas, vendo la diferencia de lo que ya tengo
b1_3 = (a_3[1,1] - a_3[2,2])*res_3$arbolSubyacente[2,2]
#en t=2 de nuevo baja
arbi_3 = b0_3*exp(r_3*T_3) + b1_3*exp(r_3*dt_3) - res_3$arbolDerivado[3,3] + a_3[2,2]*res_3$arbolSubyacente[3,3]
arbi_3_en0 = arbi_3 *exp(-r_3*T_3)
arbi_3_en0
#corroboro:
arbi_3_corrob = 3-res_3$precio
arbi_3_corrob

#si precio es $1 el valor del put esta barato:
# t= 0: compro put, vendo cartera (hago todo opuesto). La tabla de a me dice de comprar 0.33 acciones, pido prC)stamo.
b0_3 = - 1 + a_3[1,1]* res_3$arbolSubyacente[1,1]
b0_3
#en t=1 baja, por lo que debo llegar a tener 0.738 de acciones compradas, compro la diferencia de lo que ya tengo
b1_3 = (a_3[2,2] - a_3[1,1])*res_3$arbolSubyacente[2,2]
b1_3
#en t=2 de nuevo baja
arbi_3 = b0_3*exp(r_3*T_3) + b1_3*exp(r_3*dt_3) + res_3$arbolDerivado[3,3] + (-a_3[2,2])*res_3$arbolSubyacente[3,3]
arbi_3_en0 = arbi_3 *exp(-r_3*T_3)
arbi_3_en0
#corroboro:
arbi_3_corrob = res_3$precio - 1
arbi_3_corrob

# ejercicio 6 FUTUROOO --------------------------------------------------------------------------------------------------
S0_6 <- 27; K_6 <- 27; tasa_6 <- (1+0.27/4)^4 -1  #la paso a continua anual: 
r_6 <- log(1+tasa_6) ; vol_6 <- 0.25; T_6 <- 6/12; n_6 <- 2 ; dt_6= T_6/n_6

precio_call_europeo_sobrefut <- function(S0, K, r, vol, T, n) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  p <- (1 - d) / (u - d)   
  arbolS <- matrix(0, nrow=n+1, ncol=n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbolS[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  arbolF <- matrix(0, nrow=n+1, ncol=n+1)
  for (j in 0:n) {
    arbolF[j+1, n+1] <- max(0, arbolS[j+1, n+1] - K)
  }
  for (i in (n-1):0) {
    for (j in 0:i) {
      arbolF[j+1, i+1] <- (p* arbolF[j+1,i+2] + (1-p) * arbolF[j+2,i+2]) * exp(-r*dt)
    }}
  return(list(precio=arbolF[1,1], arbolSubyacente=arbolS, arbolDerivado=arbolF))
}
res_6 <- precio_call_europeo_sobrefut(S0_6,K_6,r_6,vol_6,T_6,n_6)
res_6

a_por_nodo_call_sobrefut <- function(S0, K, r, vol, T, n) {
  arbol <- precio_call_europeo_sobrefut(S0, K, r, vol, T, n)
  arbolS <- arbol$arbolSubyacente
  arbolF <- arbol$arbolDerivado
  
  a_nodos <- matrix(NA, nrow=n, ncol=n)
  for (i in (n-1):0) {
    for (j in 0:i) {
      fu <- arbolF[j+1, i+2]   # valor call si sube
      fd <- arbolF[j+2, i+2]   # valor call si baja
      Su <- arbolS[j+1, i+2]   # subyacente si sube
      Sd <- arbolS[j+2, i+2]   # subyacente si baja
      a_nodos[j+1, i+1] <- (fu - fd) / (Su - Sd)}}
  return(a_nodos)
}
a_6 <- a_por_nodo_call_sobrefut(S0_6,K_6,r_6,vol_6,T_6,n_6)
a_6

#si precio es $2 el valor del call esta caro:
# t= 0: compro cartera. La tabla de a me dice de comprar 0.497 futuro (sin costo), con eso tomo prestamo o invierto.
b0_6 = 2 
b0_6
#en t=1 aumenta, por lo que debo llegar a tener 1 de acciones, compro la diferencia de lo que ya tengo
b1_6 = a_6[1,1] * (res_6$arbolSubyacente[1,2] - res_6$arbolSubyacente[1,1])
#en t=2 de nuevo sube
arbi_6 = b0_6*exp(r_6*T_6) + b1_6*exp(r_6*dt_6) - res_6$arbolDerivado[1,3] + a_6[1,2] * (res_6$arbolSubyacente[1,3] - res_6$arbolSubyacente[1,2])
arbi_6_en0 = arbi_6 *exp(-r_6*T_6)
arbi_6_en0
#corroboro:
arbi_6_corrob = 2-res_6$precio
arbi_6_corrob

#ejercicio 5 ------------------------------------------------------------------------------------------------
S0_5 <- 27; K_5 <- 28; tasa_5 <- (1+0.26/4)^4 -1  #la paso a continua anual: 
r_5 <- log(1+tasa_5) ; vol_5 <- 0.2; T_5 <- 6/12; n_5 <- 2 ; dt_5= T_5/n_5
precio_put_europeo_sobrefut <- function(S0, K, r, vol, T, n) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  p <- (1 - d) / (u - d)   
  arbolS <- matrix(NA, nrow=n+1, ncol=n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbolS[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  arbolF <- matrix(NA, nrow=n+1, ncol=n+1)
  for (j in 0:n) {
    arbolF[j+1, n+1] <- max(0, K - arbolS[j+1, n+1])
  }
  for (i in (n-1):0) {
    for (j in 0:i) {
      arbolF[j+1, i+1] <- exp(-r*dt) * (p* arbolF[j+1,i+2] + (1-p) * arbolF[j+2,i+2])}}
  return(list(precio=arbolF[1,1], arbolSubyacente=arbolS, arbolDerivado=arbolF))
}
res_5 = precio_put_europeo_sobrefut(S0_5,K_5,r_5,vol_5,T_5,n_5)
res_5

ayb_por_nodo_put_sobrefut <- function(S0, K, r, vol, T, n) {
  # Arboles de subyacente y derivado
  arbol <- precio_put_europeo_sobrefut(S0, K, r, vol, T, n)
  arbolS <- arbol$arbolSubyacente
  arbolF <- arbol$arbolDerivado
  
  a_nodos <- matrix(NA, nrow=n, ncol=n)
  b_nodos <- matrix(NA, nrow=n, ncol=n)
  dt <- T/n
  
  for (i in (n-1):0) {
    for (j in 0:i) {
      fu <- arbolF[j+1, i+2]   # valor put si sube
      fd <- arbolF[j+2, i+2]   # valor put si baja
      Su <- arbolS[j+1, i+2]   # subyacente si sube
      Sd <- arbolS[j+2, i+2]   # subyacente si baja
      
      # a (delta)
      a_nodos[j+1, i+1] <- (fu - fd) / (Su - Sd)
      
      # b (bono libre de riesgo)
      b_nodos[j+1, i+1] <- (fu - a_nodos[j+1, i+1] * Su) * exp(-r*dt)
    }
  }
  return(list(a=a_nodos, b=b_nodos))
}
ayb_5 = ayb_por_nodo_put_sobrefut(S0_5,K_5,r_5,vol_5,T_5,n_5)
#si precio es $2 el valor del put esta caro:
# t= 0: vendo put, compro cartera. La tabla de a me dice de vender 0.5284 acciones, con lo recaudado invierto.
b0_5 = 2 - a_5[1,1]* res_5$arbolSubyacente[1,1]
b0_5
#en t=1 baja, por lo que debo llegar a tener 1 de acciones vendidas, vendo la diferencia de lo que ya tengo
b1_5 = (a_5[1,1] - a_5[2,2])*res_5$arbolSubyacente[2,2]
b1_5
#en t=2 de nuevo baja
arbi_5 = b0_5*exp(r_5*T_5) + b1_5*exp(r_5*dt_5) - res_5$arbolDerivado[3,3]  + a_5[2,2]*res_5$arbolSubyacente[3,3]
arbi_5_en0 = arbi_5 *exp(-r_5*T_5)
arbi_5_en0
#corroboro:
arbi_5_corrob = 2-res_5$precio
arbi_5_corrob

#c uso paridad Call-Put
primacall = exp(-r_5*T_5)*(S0_5-K_5)+res_5$precio  #es el precio de un futuro, por eso se descuento S0
primacall
#corroboro
precio_call_europeo_sobrefut <- function(S0, K, r, vol, T, n) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  p <- (1 - d) / (u - d)   
  arbolS <- matrix(NA, nrow=n+1, ncol=n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbolS[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  arbolF <- matrix(NA, nrow=n+1, ncol=n+1)
  for (j in 0:n) {
    arbolF[j+1, n+1] <- max(0, arbolS[j+1, n+1] - K)
  }
  for (i in (n-1):0) {
    for (j in 0:i) {
      arbolF[j+1, i+1] <- exp(-r*dt) * (p* arbolF[j+1,i+2] + (1-p) * arbolF[j+2,i+2])}}
  return(list(precio=arbolF[1,1], arbolSubyacente=arbolS, arbolDerivado=arbolF))
}
res_5_c <- precio_call_europeo_sobrefut(S0_5,K_5,r_5,vol_5,T_5,n_5)
res_5_c


#ejercicio 7------------------------------------------------------------------------------------------
S0_7 <- 40; K_7 <- 42; r_7 <- 0.3; vol_7 <- 0.25; T_7 <- 4/12; n_7 <- 2 ; dt_7= T_7/n_7 ; extr_7 = 0.05
precio_put_europeo_extr <- function(S0, K, r, vol, T, n, extr) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  p <- (exp((r-extr)*dt) - d) / (u - d)   
  arbolS <- matrix(NA, nrow=n+1, ncol=n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbolS[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  arbolF <- matrix(NA, nrow=n+1, ncol=n+1)
  for (j in 0:n) {
    arbolF[j+1, n+1] <- max(0, K - arbolS[j+1, n+1])
  }
  for (i in (n-1):0) {
    for (j in 0:i) {
      arbolF[j+1, i+1] <- exp(-r*dt) * (p* arbolF[j+1,i+2] + (1-p) * arbolF[j+2,i+2])}}
  return(list(precio=arbolF[1,1], arbolSubyacente=arbolS, arbolDerivado=arbolF))
}
res_7 <- precio_put_europeo_extr(S0_7,K_7,r_7,vol_7,T_7,n_7,extr_7)
res_7 

a_por_nodo_put_extr <- function(S0, K, r, vol, T, n,extr,dt) {
  arbol <- precio_put_europeo_extr(S0, K, r, vol, T, n,extr)
  arbolS <- arbol$arbolSubyacente
  arbolF <- arbol$arbolDerivado
  dt = T/n
  
  a_nodos <- matrix(NA, nrow=n, ncol=n)
  for (i in (n-1):0) {
    for (j in 0:i) {
      fu <- arbolF[j+1, i+2]   # valor call si sube
      fd <- arbolF[j+2, i+2]   # valor call si baja
      Su <- arbolS[j+1, i+2]   # subyacente si sube
      Sd <- arbolS[j+2, i+2]   # subyacente si baja
      a_nodos[j+1, i+1] <- (fu - fd) / ((Su - Sd) * exp(extr * dt))}}
  return(a_nodos)
}
a_7 <- a_por_nodo_put_extr(S0_7,K_7,r_7,vol_7,T_7,n_7,extr_7)

#si precio es $1.5 el valor del put esta barato:
# t= 0: compro put, vendo cartera. La tabla de a me dice de comprar 0.4927 de moneda extranjera.
b0_7 = res_7$arbolDerivado[1,1] - a_7[1,1]* res_7$arbolSubyacente[1,1]
flujo0_7 = a_7[1,1]* res_7$arbolSubyacente[1,1] + b0_7 - 1.5
flujo0_7
#en t=1 sube, por lo que debo llegar a tener 0.21 de moneda compradas, baja la cant. Vendo la diferencia.
cant_moneda = (a_7[1,2] - a_7[1,1]*exp(dt_7*extr_7))*res_7$arbolSubyacente[1,2] #esta cantidad recibo en moneda local por vender, flujo
b1_7 = res_7$arbolDerivado[1,2] - a_7[1,2]* res_7$arbolSubyacente[1,2]
flujo1_7 = cant_moneda + b1_7 - b0_7*exp(r_7*dt_7)
flujo1_7
#en t=2 baja
flujo2_7 = res_7$arbolDerivado[2,3] - b1_7*exp(r_7*dt_7) + (-a_7[1,2]*exp(extr_7*dt_7)*res_7$arbolSubyacente[2,3])
flujo2_7
#corroboro:
arbi_7_corrob = res_7$precio - 1.5
arbi_7_corrob

precio_put_americano_extr <- function(S0, K, r, vol, T, n, extr) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  p <- (exp((r-extr)*dt) - d) / (u - d)   # probabilidad riesgo neutro ajustada por r_extr
  # Crbol del subyacente (extrisa)
  arbolS <- matrix(NA, nrow=n+1, ncol=n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbolS[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  # Crbol del derivado
  arbolF <- matrix(NA, nrow=n+1, ncol=n+1)
  # Payoffs en el vencimiento
  for (j in 0:n) {
    arbolF[j+1, n+1] <- max(0, K - arbolS[j+1, n+1])
  }
  # Backward induction
  for (i in (n-1):0) {
    for (j in 0:i) {
      # Valor si continC:a
      continuar <- exp(-r*dt) * (p * arbolF[j+1,i+2] + (1-p) * arbolF[j+2,i+2])
      # Valor si ejerce ahora
      ejercer <- max(0, K - arbolS[j+1,i+1])
      # En americano se toma el mC!ximo
      arbolF[j+1,i+1] <- max(continuar, ejercer)}}
  return(list(precio=arbolF[1,1], arbolSubyacente=arbolS, arbolDerivado=arbolF))
}
res_7e <- precio_put_americano_extr(S0_7, K_7, r_7, vol_7, T_7, n_7, extr_7)
res_7e
prima = res_7$precio - res_7e$precio
prima

C <- res_7$precio + S0_7*exp(-extr_7*T_7) - K_7*exp(-r_7*T_7)
C

#ejercicio 9 -----------------------------------------------------------------------------------------
S0_9 <- 50; K_9 <- 51; r_9 <- 0.08; vol_9 <- 0.25; T_9 <- 8/12; n_9 <- 2 ; dt_9= T_9/n_9 ; div_9 = 0.05
precio_put_europeo_div <- function(S0, K, r, vol, T, n, div) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  p <- (exp((r-div)*dt) - d) / (u - d)   
  arbolS <- matrix(NA, nrow=n+1, ncol=n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbolS[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  arbolF <- matrix(NA, nrow=n+1, ncol=n+1)
  for (j in 0:n) {
    arbolF[j+1, n+1] <- max(0, K - arbolS[j+1, n+1])
  }
  for (i in (n-1):0) {
    for (j in 0:i) {
      arbolF[j+1, i+1] <- exp(-r*dt) * (p* arbolF[j+1,i+2] + (1-p) * arbolF[j+2,i+2])}}
  return(list(precio=arbolF[1,1], arbolSubyacente=arbolS, arbolDerivado=arbolF))
}
res_9 <- precio_put_europeo_div(S0_9,K_9,r_9,vol_9,T_9,n_9,div_9)
res_9 

a_por_nodo_put_div <- function(S0, K, r, vol, T, n,div,dt) {
  arbol <- precio_put_europeo_div(S0, K, r, vol, T, n,div)
  arbolS <- arbol$arbolSubyacente
  arbolF <- arbol$arbolDerivado
  dt = T/n
  
  a_nodos <- matrix(NA, nrow=n, ncol=n)
  for (i in (n-1):0) {
    for (j in 0:i) {
      fu <- arbolF[j+1, i+2]   # valor call si sube
      fd <- arbolF[j+2, i+2]   # valor call si baja
      Su <- arbolS[j+1, i+2]   # subyacente si sube
      Sd <- arbolS[j+2, i+2]   # subyacente si baja
      a_nodos[j+1, i+1] <- (fu - fd) / ((Su - Sd) * exp(div * dt))}}
  return(a_nodos)
}
a_9 <- a_por_nodo_put_div(S0_9,K_9,r_9,vol_9,T_9,n_9,div_9)
a_9

#si precio es $3.2 el valor del put esta barato:
# t= 0: compro put, vendo cartera. La tabla de a me dice de comprar 0.4485 de moneda extranjera.
b0_9 = res_9$arbolDerivado[1,1] - a_9[1,1]* res_9$arbolSubyacente[1,1]
flujo0_9 = a_9[1,1]* res_9$arbolSubyacente[1,1] + b0_9 - 3.2
flujo0_9
#en t=1 sube, por lo que debo llegar a tener 0.21 de moneda compradas, baja la cant. Vendo la diferencia.
cant_moneda = (a_9[1,2] - a_9[1,1]*exp(dt_9*div_9))*res_9$arbolSubyacente[1,2] #esta cantidad recibo en moneda local por vender, flujo
b1_9 = res_9$arbolDerivado[1,2] - a_9[1,2]* res_9$arbolSubyacente[1,2]
flujo1_9 = cant_moneda + b1_9 - b0_9*exp(r_9*dt_9)
flujo1_9
#en t=2 baja
flujo2_9 = res_9$arbolDerivado[2,3] - b1_9*exp(r_9*dt_9) + (-a_9[1,2]*exp(div_9*dt_9)*res_9$arbolSubyacente[2,3])
flujo2_9
#corroboro:
arbi_9_corrob = res_9$precio - 3.2
arbi_9_corrob

#ejercicio 12 -------------------------------------------------------------------------------------------
S0_12 <- 20; K_12 <- 21; tea_12 <- (1+0.3/3)^3 -1 ;r_12 = log(1+tea_12);vol_12 <- 0.25; T_12 <- 8/12; n_12 <- 2 ; dt_12= T_12/n_12 
rend_tea = (1+0.15/3)^3-1 ; rend_12 = log(1+rend_tea)
precio_call_europeo_rend <- function(S0, K, r, vol, T, n, rend) {
  dt <- T/n
  u <- exp(vol * sqrt(dt))
  d <- exp(-vol * sqrt(dt))
  p <- (exp((r-rend)*dt) - d) / (u - d)   
  arbolS <- matrix(NA, nrow=n+1, ncol=n+1)
  for (i in 0:n) {
    for (j in 0:i) {
      arbolS[j+1, i+1] <- S0 * u^(i-j) * d^j}}
  arbolF <- matrix(NA, nrow=n+1, ncol=n+1)
  for (j in 0:n) {
    arbolF[j+1, n+1] <- max(0, arbolS[j+1, n+1] - K)
  }
  for (i in (n-1):0) {
    for (j in 0:i) {
      arbolF[j+1, i+1] <- exp(-r*dt) * (p* arbolF[j+1,i+2] + (1-p) * arbolF[j+2,i+2])}}
  return(list(precio=arbolF[1,1], arbolSubyacente=arbolS, arbolDerivado=arbolF))
}
res_12 <- precio_call_europeo_rend(S0_12,K_12,r_12,vol_12,T_12,n_12,rend_12)
res_12 

a_por_nodo_call_rend <- function(S0, K, r, vol, T, n,rend,dt) {
  arbol <- precio_put_europeo_rend(S0, K, r, vol, T, n,rend)
  arbolS <- arbol$arbolSubyacente
  arbolF <- arbol$arbolDerivado
  dt = T/n
  
  a_nodos <- matrix(NA, nrow=n, ncol=n)
  for (i in (n-1):0) {
    for (j in 0:i) {
      fu <- arbolF[j+1, i+2]   # valor call si sube
      fd <- arbolF[j+2, i+2]   # valor call si baja
      Su <- arbolS[j+1, i+2]   # subyacente si sube
      Sd <- arbolS[j+2, i+2]   # subyacente si baja
      a_nodos[j+1, i+1] <- (fu - fd) / ((Su - Sd) * exp(rend * dt))}}
  return(a_nodos)
}
a_12 <- a_por_nodo_call_rend(S0_12,K_12,r_12,vol_12,T_12,n_12,rend_12)
a_12

#si precio es $2 el valor del call esta caro:
# t= 0: vendo call, compro cartera. La tabla de a me dice de vender 0.37 de acciones.
b0_12 = res_12$arbolDerivado[1,1] - a_12[1,1]* res_12$arbolSubyacente[1,1]
flujo0_12 = -a_12[1,1]* res_12$arbolSubyacente[1,1] - b0_12 + 2
flujo0_12
#en t=1 baja, por lo que debo llegar a tener 0.95 de acciones vendidas, aumento la cant. Vendo la diferencia.
cant_moneda = (a_12[1,1]*exp(dt_12*rend_12) - a_12[2,2])*res_12$arbolSubyacente[2,2] #esta cantidad recibo en moneda local por vender, flujo
b1_12 = res_12$arbolDerivado[2,2] - a_12[2,2]* res_12$arbolSubyacente[2,2]
flujo1_12 = cant_moneda - b1_12 + b0_12*exp(r_12*dt_12)
flujo1_12

#en t=2 sube
flujo2_12 = - res_12$arbolDerivado[1,3] + b1_12*exp(r_12*dt_12) + a_12[2,2]*exp(rend_12*dt_12)*res_12$arbolSubyacente[1,3]
flujo2_12
#no daaaaaaaaaaa
#corroboro:
arbi_12_corrob = res_12$precio - 3.2
arbi_12_corrob

