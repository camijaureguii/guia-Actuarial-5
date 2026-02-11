#Bull spread calls con compra K1 y venta K2, con k1<k2
#install.packages("ggplot2")
library(ggplot2)
opciones <- data.frame(
  Opcion = c("Call_1","Call_2","Call_3","Put_1","Put_2","Put_3"),
  Strike = c(30, 35, 40, 30, 35, 40),
  Prima  = c(3.00, 1.50, 0.75, 1.75, 2.50, 5.00),
  Tipo   = c("Call","Call","Call","Put","Put","Put")
)
c1 <- opciones[opciones$Opcion=="Call_1", ]
c2 <- opciones[opciones$Opcion=="Call_2", ]
c3 <- opciones[opciones$Opcion=="Call_3", ]
p1 <- opciones[opciones$Opcion=="Put_1", ]
p2 <- opciones[opciones$Opcion=="Put_2", ]
p3 <- opciones[opciones$Opcion=="Put_3", ]

BullSpread_Call <- function(K1, K2, Prima1, Prima2,n){
  St <- seq(0, n, by = 0.5)
  rdo <- ifelse(St < K1,
                Prima2 - Prima1,
                ifelse(St < K2,
                       Prima2 - Prima1 + St - K1,
                       Prima2 - Prima1 + K2 - K1))
  df = data.frame(St, rdo)
  gg_fx = ggplot(df, aes(x=St, y=rdo)) +
    geom_line(colour="blue") +
    geom_hline(yintercept=0, linetype=2) +
    geom_vline(xintercept=K1, linetype=2, colour="red") +
    geom_vline(xintercept=K2, linetype=2, colour="red") 
  return(gg_fx)} 
#el que tiene menor k es el que tiene que ser compra/long y va como K1
BullSpread_Call(c1$Strike, c2$Strike, c1$Prima, c2$Prima,50)
BullSpread_Call(c2$Strike, c3$Strike, c2$Prima, c3$Prima,50)

BearSpread_Call <- function(K1, K2, Prima1, Prima2,n){
  St <- seq(0, n, by = 0.5)
  rdo <- ifelse(St < K1,
                Prima1 - Prima2,
                ifelse(St < K2,
                       Prima1 - Prima2 + K1 - St,
                       Prima1 - Prima2 + K1 - K2))
  df = data.frame(St, rdo)
  gg_fx = ggplot(df, aes(x=St, y=rdo)) +
    geom_line(colour="blue") +
    geom_hline(yintercept=0, linetype=2) +
    geom_vline(xintercept=K1, linetype=2, colour="red") +
    geom_vline(xintercept=K2, linetype=2, colour="red") 
  return(gg_fx)} 

#el que tiene menor k es el que tiene que ser venta/short y va como K1
BearSpread_Call(c2$Strike, c3$Strike, c2$Prima, c3$Prima,50)

BearSpread_Put <- function(K1, K2, Prima1, Prima2,n){
  St <- seq(0, n, by = 0.5)
  rdo <- ifelse(St < K1,
                Prima1 - Prima2 + K2 - K1,
                ifelse(St < K2,
                       Prima1 - Prima2 + K2 - St,
                       Prima1 - Prima2))
  df = data.frame(St, rdo)
  gg_fx = ggplot(df, aes(x=St, y=rdo)) +
    geom_line(colour="blue") +
    geom_hline(yintercept=0, linetype=2) +
    geom_vline(xintercept=K1, linetype=2, colour="red") +
    geom_vline(xintercept=K2, linetype=2, colour="red") 
  return(gg_fx)
} #el que tiene menor k es el que tiene que ser venta/short y va como K1
BearSpread_Put(p1$Strike, p2$Strike, p1$Prima, p2$Prima,50)
BearSpread_Put(p2$Strike, p3$Strike, p2$Prima, p3$Prima,50)

BullSpread_Put <- function(K1, K2, Prima1, Prima2){
  St <- seq(0, 50, by = 0.5)
  rdo <- ifelse(St < K1,
                Prima2 - Prima1 + K1 - K2,
                ifelse(St < K2,
                       Prima2 - Prima1 + St  - K2,
                       Prima2 - Prima1))
  df = data.frame(St, rdo)
  gg_fx = ggplot(data = df, aes(x = St, y = rdo)) + geom_line(colour="blue") + geom_hline(yintercept=0, linetype = 2) + geom_vline(xintercept=0, linetype=2)
  return(gg_fx)
} #el que tiene menor k es el que tiene que ser compra/long y va como K1
BullSpread_Put(p2$Strike, p3$Strike, p2$Prima, p3$Prima)


# Función para detectar arbitraje
ArbitrajeCall <- function(S0, K, r, T, prima_merc){
  precio_min <- max(0, S0 - K * exp(-r * T))
  precio_max <- S0 # Cota superior 
  if (prima_merc < precio_min){
    ganancia <- precio_min - prima_merc
    return(paste("Arbitraje: el call está demasiado barato.",
                 "Comprar el call genera ganancia segura de", round(ganancia, 5)))
    #compro call, vendo subyacente e invierto, ejerzo el call en T. 
  } else if (prima_merc > precio_max){
    ganancia_min <- prima_merc - precio_max
    return(paste("Arbitraje: el call está demasiado caro.",
                 "Vender el call genera ganancia segura de", round(ganancia_min, 5)))
    #vendo call y compro subyacente, como mucho obtengo K en T.
  } else {
    return("No hay arbitraje: la prima está entre las cotas mínima y máxima")
  }}
ArbitrajeCall(33,30,0.12,3/12,2)
#5 - 33 + 30*exp(-0.12*3/12) ejemplo que si la prima es 5 es mayor a la cota mínima pero dependo de que St 
#sea mayor que K en T para no recurrir en pérdidas

ArbitrajePut <- function(S0, K, r, T, prima_merc){
  precio_min <- max(0, K * exp(-r * T) - S0)   # cota inferior
  precio_max <- K * exp(-r * T)                # cota superior
  if (prima_merc < precio_min){
    ganancia <- precio_min - prima_merc
    return(paste("Arbitraje: la put está demasiado barata.",
                 "Comprar la put y cubrir genera ganancia segura de", round(ganancia, 2)))
    #compro subyancente con préstamo, compro put, ejerzo put en T y con eso pago préstamo.
  } else if (prima_merc > precio_max){
    ganancia_min <- prima_merc - precio_max
    return(paste("Arbitraje: la put está demasiado cara.",
                 "Vender la put genera ganancia mínima inmediata de", round(ganancia_min, 2),
                 "ya que el máximo que pagás al vencimiento es K."))
    #vendo put e invierto y como mucho pago K en T
  } else {
    return("No hay arbitraje: la prima está entre las cotas mínima y máxima")
  }}
ArbitrajePut(33,40,0.12,3/12,5)

ArbitrajeCall(34,30,0.08,2/12,3)
ArbitrajePut(34,40,0.08,2/12,5)

#Ejercicio 2
opciones2 <- data.frame(
  Opcion = c("Call_1","Call_2","Call_3","Put_1","Put_2","Put_3"),
  Tipo   = c("Call","Call","Call","Put","Put","Put"),
  Strike = c(35, 45, 55, 35, 45, 55),
  Prima  = c(9.00, 4.50, 2.00, 2.00, 5.00, 10.00)
)

c1_2 <- opciones2[opciones2$Opcion=="Call_1", ]
c2_2 <- opciones2[opciones2$Opcion=="Call_2", ]
c3_2 <- opciones2[opciones2$Opcion=="Call_3", ]
p1_2 <- opciones2[opciones2$Opcion=="Put_1", ]
p2_2 <- opciones2[opciones2$Opcion=="Put_2", ]
p3_2 <- opciones2[opciones2$Opcion=="Put_3", ]

BullSpread_Call(c1_2$Strike, c3_2$Strike, c1_2$Prima, c3_2$Prima,65)
BearSpread_Put(p1_2$Strike, p3_2$Strike, p1_2$Prima, p3_2$Prima,65)

ArbitrajeCall(44,35,0.05,3/12,9)
ArbitrajePut(44,55,0.05,3/12,10)
