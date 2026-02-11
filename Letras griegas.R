#Ejercicio 1
mu_1 = 0.25 ; sigma_1 = 0.35 ; r_1 = 0.07 ; T_1 = 3/12 ; K_1 = 250 ; S0_1 = 250 ; multC_1 = 100 
T_fut_1 = 4/12 ; multF_1 = 50 ; q_1= 0

#a) Vendo una call para el indice de mercado, al vencimiento me ejercen si el indice en el mercado está más caro
#por cada punto de diferencia en el índice, se paga 100. ej: 100 (IndVencim - IndStrike)  si es positivo pago eso, si es negativo no se ejerce y gané prima

F0 = S0_1 * exp(r_1*T_fut_1) #valor del contrato de futuros, precio de referencia para ver quien cobra

#Para un call, Δcall>0 (el valor del call sube si sube el índice). Para mi que estoy corta del call, mi delta es negativo.
#Por otro lado, los futuros sí se mueven directamente con el índice. Entonces, si estoy corta del call,
#para compensar necesito una posición larga en futuros (delta positiva).

#calculo delta del call
d1 <- (log(S0_1 / K_1) + (r_1 + 0.5 * sigma_1^2) * T_1) / (sigma_1 * sqrt(T_1))
delta_call <- pnorm(d1)   # Delta de la call ATM
delta_call_mult = delta_call * multC_1

#calculo delta del futuro
delta_fut= exp(r_1*T_fut_1)
delta_fut_mult = delta_fut * multF_1

# Calculo cantidad de contratos de futuros a comprar: -deltacall*cant_call + deltafut*cant_fut = 0
cantidad_futuros_1 <- delta_call_mult / delta_fut_mult  
cantidad_futuros_1

#b) Para que la periodicidad de ajuste sea menor, el gamma, que nos indica el cambio de delta, tiene que ser menor o cero
#Si ∣Γ∣ es grande, cualquier movimiento chico en S hace que tu delta se descalibre enseguida ⇒ tenés que rebalancear muy seguido.
#Si ∣Γ∣ es chico (o 0), el delta casi no cambia cuando se mueve S ⇒ podés ajustar con menos frecuencia.
gamma_put = 35.8586 ; S0_p = 255 ; multP_1 = 100
gamma <- (dnorm(d1) * exp(-q_1 * T_1)) / (S0_1 * sigma_1 * sqrt(T_1))
gamma_call_mult <- gamma * multC_1 
gamma_call_mult

gamma_put_mult = gamma_put #está ya en el instrumento el multiplcador 

# Planteo: -gammacall*cant_call + gammafut*cant_fut + gamma_put*cant_put = 0 
# y si gammafut=0 siempre 
cant_put <- gamma_call_mult / gamma_put_mult
cant_put

#c)
z_95 <- qnorm(0.95)
ST_95 <- S0_1 * exp((mu_1 - 0.5 * sigma_1^2) * T_1 + sigma_1 * sqrt(T_1) * z_95)  
#ecuacion del movimiento geometrico browniano, en funcion del Z95
perdida_95 <- -max(ST_95 - K_1, 0) * multC_1
ST_95
perdida_95

#-------------------------------------------------------------------------------------------------------
#Ejercicio 2
valor_cartera = 12000000 ; beta = 1.2 ; S0 = 1300 ; sigma  = 0.25 ; q = 0.05 ; r = 0.07 ; mult= 10 ; t = 1
# delta_cartera = beta * delta_indice
#teniendo en cuenta eso, calculo cual es el porcentaje de variación del índice que hace que mi cartera baje un 10%
max_porc_indice = 0.1 / beta #quiero que baje hasta un 10% pero mi indice es un 20% ergo debe bajar hasta 8.33%
max_porc_indice #porcentaje maximo que puede bajar el indice

#considerando esto, calculo el precio de strike, el que fijo para poder vender a ese precio, cosa de que si 
#sigue cayendo no sigo perdiendo

K = S0 * (1-max_porc_indice)
K

#calculo el precio teorico para un put ya que es el instrumento te da ganancias cuando el mercado cae

d1 <- (log(S0/K) + (r - q + 0.5 * sigma^2) * t) / (sigma * sqrt(t))
d2 <- d1 - sigma * sqrt(t)

put_bs <- K * exp(-r * t) * pnorm(-d2) - S0 * exp(-q * t) * pnorm(-d1)
put_bs

#calculo cuantos contratos de put necesitaria para esta cartera

cantidad_puts <- valor_cartera * beta / (S0)
costo_total <- put_bs * cantidad_puts
costo_total #da distinto a la guia

#b) La cobertura es estática, ya que se fija al inicio mediante la compra de puts sobre el índice con vencimiento 
#a un año, y no requiere rebalanceo periódico.

#c)
delta_put = exp(-q*t)*-pnorm(-d1) #Es negativo porque los puts ganan valor cuando baja el índice (protegen ante caídas).
delta_total = delta_put * cantidad_puts
delta_fut = exp((r - q) * 13/12)

cant_fut = delta_total / delta_fut
cant_fut #para tener la misma cobertura que el put, debo tener una posición vendida de 3043 futuros

#d) Dinámica. En (c) igualaste deltas al inicio con una posición corta en futuros. Como el put del (a) 
# tiene gamma y vega (cambian su delta con S,t y σ), para seguir replicándolo con futuros (que tienen 
#delta ~1 y gamma 0) hay que recalcular y rebalancear la cantidad de contratos a lo largo del tiempo

#-----------------------------------------------------------------------------------------------------------------
#3)
r = 0.05 ; S0 = 20 ; t_call = 0.5 ; K_call = 21 ; gamma_call = 0.1127 ; t_fut = 8/12 ; t_put = 3/12
K_put = 20 ; mu = 0.2 ; sigma = 0.25

#a)La probabilidad de que se ejerza la call será la P(St > K_call) = 1- P(St < K_call)
#sabemos que St = S0*exp((u- 0.5*σ^2)*t + σ*Z*sqrt(t)), reemplazo eso arriba y aplico ln() en ambos lados
#ln(S0) + ((μ−0.5σ^2)T + σ*sqrt(T)Z) > ln(K)
#despejo la Z Y calculo la probabilidad en una normal estandar de eso:

d <- (log(K_call/S0) - (mu - 0.5 * sigma^2) * t_call) / (sigma * sqrt(t_call))
prob_ejercicio <- 1 - pnorm(d)
prob_ejercicio

#b) Yo vendo el put y quiero armar una cobertura usando solo el activo subyacente. Eso es una cobertura por delta.
d1_p = (log(S0/K_put)+(r+0.5*sigma^2)*t_put)/(sigma*sqrt(t_put))
N_d1_p= pnorm(-d1_p)
delta_put = -N_d1_p
delta_put

#Tu delta actual (por estar corta en el put) es +0.4355 (signo contrario)
#Para anularlo necesitás tomar una posición en el subyacente que aporte -0.4355.
#Si vos vendés 0.4355 unidades del subyacente, esa posición tiene delta = −0.4355

#c) Para que sea delta y gamma neutral:
#en el call, busco su delta, gamma ya lo tenemos:
d1_c = (log(S0/K_call)+(r+0.5*sigma^2)*t_call)/(sigma*sqrt(t_call))
delta_call = pnorm(d1_c)
delta_call #0.48

#en el futuro busco su delta nomas, no tiene gamma =0
delta_fut = exp(r*t_fut)
gamma_fut = 0

#Busco gamma del put:
gamma_put = dnorm(d1_p)/(S0*sigma*sqrt(t_put))
gamma_put #estoy vendida entonces es -0.1574


A <- matrix(c(delta_call, gamma_call,          # -deltaput*cant_put + deltafut*cant_fut + deltacall*cant_call = 0
              delta_fut, gamma_fut), ncol = 2) # -gammaput*cant_put + gammafut*cant_fut + gammacall*cant_call = 0

b <- c(delta_put, gamma_put) #no esta en negativo porque pasó al otro lado de la igualdad

cantidades <- solve(A, b) 
cantidades #fijarse porque da distinto los signos

#d) 
epsilon =0.3 ; t_sim = 6/12
S_sim = S0*exp((mu - 0.5*sigma^2)*t_sim + sigma*epsilon*t_sim)
S_sim # el valor del activo simulado para dentro de 6 meses es de $22.59226

callsim = max(0, S_sim - K_call)
callsim

#-------------------------------------------------------------------------------------------------------
#Ejercicio 4
rf_4 = 0.03 ; sigma_4 = 0.2 ; S0_4 = 45 ; r_4 = 0.1 ; mu_4 = 0.15 ; t_4 = 4/12 ; cant_local = 10000

### a
# una devaluación del 10% indica que el tipo de cambio sea $49.5
K_4 = 45*1.1
K_4
# valúo la prima de un call (ya que me perjudica cuando S0 aumenta, por lo que debo comprar un call para cubrirme) 
#con K = 49.5, que me permita comprar la divisa a ese precio

d1 <- (log(S0_4/K_4) + (r_4 - rf_4 + 0.5 * sigma_4^2) * t_4) / (sigma_4 * sqrt(t_4))
d2 <- d1 - sigma_4 * sqrt(t_4)

call_4 <- S0_4 * exp(-rf_4 * t_4) * pnorm(d1) - K_4 * exp(-r_4 * t_4) * pnorm(d2)  
call_4

costo4 = call_4*cant_local/S0_4  #cada call cubre una moneda extranjera
costo4

#b)
delta_call = exp(-rf_4 * t_4) * pnorm(d1)
delta_call_total = delta_call * cant_local / S0_4 #en moneda extranjera, como lo vendi (vemos al que lo emite) 
# es el signo contrario

#busco el delta del futuro
delta_fut_4 = exp((r_4 - rf_4)*12/12)
delta_fut_4

# ahora, sabiendo que un futuro tiene delta de 1.023608, debo conocer la cantidad de contratos necesarios 
#para anular el delta: -deltacall*cant_call + deltafut*cant_fut = 0  
cant_fut_4 = delta_call_total/delta_fut_4
cant_fut_4 #compro futuros

#------------------------------------------------------------------------------------------------------------
#Ejercicio 5
S0_5 = 20 ; q_5 = 0.05 ; tend_5 = 0.25 ; vol_5 = 0.2 ; r_5 = 0.08
# debo simular el camino de precios de la acción, para luego poder simular el de futuros
# hago una simulación de 1000 caminos
# la prima del call es igual al valor actual del promedio de los payoffs simulados
precios5 = matrix(NA, nrow = 1000, ncol = 20)
precios5[,1] = S0_5
futuros5 = matrix(NA, nrow = 1000, ncol = 20)
futuros5[,1] = S0_5*exp(r_5 - q_5)
call5 = matrix(NA, nrow = 1000)

set.seed(2112)
for (i in 1:1000){
  for (j in 2:20){
    precios5[i,j] = precios5[i,j-1]*exp((tend_5 - 0.5*vol_5^2)/365 + vol_5*sqrt(1/365)*rnorm(1))
    futuros5[i,j] = precios5[i,j]*exp((r_5 - q_5)*(365 - j + 1)/365)
  }
  call5[i] = max(0, mean(futuros5[i, 18:20]) - futuros5[i, 1])
}
head(precios5)
head(futuros5)
# calculo ahora el valor actual del promedio de los payoffs
call_5 = mean(call5)*exp(-r_5*20/365)
call_5

#----------------------------------------------------------------------------------------------------------
#Ejercicio 6
cart_6 = 10000000 ; beta_6 = 1.4 ; S0_6 = 1500 ; sigma_6 = 0.18 ; qc_6 = 0.04 ; qi_6 = 0.06 ; r_6 = 0.08 ;t_6=1
max_porc_indice = 0.1 / beta_6

K_6 = S0_6 * (1-max_porc_indice-qi_6)
K_6

#calculo el precio teorico para un put ya que es el instrumento te da ganancias cuando el mercado cae
d1 <- (log(S0_6/K_6) + (r_6 - qi_6 + 0.5 * sigma_6^2) * t_6) / (sigma_6 * sqrt(t_6))
d2 <- d1 - sigma_6 * sqrt(t_6)

put_bs <- K_6 * exp(-r_6 * t_6) * pnorm(-d2) - S0_6 * exp(-qi_6 * t_6) * pnorm(-d1)
put_bs

cant_opc_6 = beta_6 * cart_6 / S0_6
cant_opc_6*put_bs # DA MAL

#------------------------------------------------------------------------------------------------
#Ejercicio 7
q_7 = 0.04 ; mu_7 = 0.1 ; r_7 = 0.06 ; S0_7 = 80 ; sigma_7 = 0.25 ; tp_7 = 9/12 ; tf_7 = 1 

precio_fut = S0_7 * exp(r_7 - q_7)
K_7= precio_fut

d1_7 <- (log(K_7/K_7) + ( 0.5 * sigma_7^2) * tp_7) / (sigma_7 * sqrt(tp_7))
d2_7 <- d1_7 - sigma_7 * sqrt(tp_7)

put_bs_7 <- K_7 * exp(-r_7 * tp_7) * pnorm(-d2_7) - K_7 * pnorm(-d1_7)
put_bs_7

#b) # para saber la probabilidad de que el put sea ejercido, necesito calcular N(-d2) 
prob7_b = pnorm(-d2_7)
prob7_b

d <- (log(K_7/S0_7) - (mu_7 - 0.5 * sigma_7^2) * tp_7) / (sigma_7 * sqrt(tp_7))
prob_ejercicio <- pnorm(d)
prob_ejercicio           #preguntar cuando uso una y la otraaaaaaa

#------------------------------------------------------------------------------------------------------
#Ejercicio 8
rf_8 = 0.04 ; sigma_8 = 0.25 ; S0_8 = 40 ; r_8 = 0.12 ; mu_8 = 0.2 ; t_tc = 3/12
K_8 = S0_8*0.95
K_8
# una reducción del tipo de cambio de un 5% equivale a que el mismo sea de 38
# debo cubrirme comprando un put con K = 38
d1_8 <- (log(S0_8/K_8) + ( r_8 - rf_8 + 0.5 * sigma_8^2) * t_tc) / (sigma_8 * sqrt(t_tc))
d2_8 <- d1_8 - sigma_8 * sqrt(t_tc)

put_bs_8 <- K_8 * exp(-r_8 * t_tc) * pnorm(-d2_8) - S0_8 * exp(-rf_8 * t_tc) * pnorm(-d1_8)
put_bs_8 

costo = put_bs_8 * 1000 *S0_8  #raro, no entiendo por que multiplica por S0_8
costo

#b) La probabilidad de que el TC sea menor a 38$ es P(St_8 < K_8) = P(St_8 < K_8)
#sabemos que St = S0*exp((u- 0.5*σ^2)*t + σ*Z*sqrt(t)), reemplazo eso arriba y aplico ln() en ambos lados
#ln(S0) + ((μ−0.5σ^2)T + σ*sqrt(T)Z) > ln(K)
#despejo la Z Y calculo la probabilidad en una normal estandar de eso:
d <- (log(K_8/S0_8) - (mu_8 - 0.5 * sigma_8^2) * t_tc) / (sigma_8 * sqrt(t_tc))
prob_ejercicio <- pnorm(d)
prob_ejercicio





