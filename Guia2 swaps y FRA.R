#Ejercicio 1
# Busco tasa forward 3x12

Tasafmonth = function(tasas, start, last){
  tasaefectlast= 1 + tasas[last]*last/(12*100)
  tasaefectstart = 1+ tasas[start]*start/(12*100)
  tasaforw = (tasaefectlast/tasaefectstart - 1) * (12) / (last-start)  #la paso a tna
  return(tasaforw*100)
}

tna <- c(4.05,4.25,4.85,5.25,5.50,5.80,5.95,6.15,6.35,6.80,6.95,7.05)

Tasafmonth(tna,3,12)
Costofijo = Tasafmonth(tna,3,12)+ 1.15
Costofijo

Valorcontrato_liq_pago_FRA = function(tasafor,tasalibor,prestamo,start,last){
  valorcontrato = (tasalibor-tasafor)/100*(last-start)/12*prestamo*(1+tasalibor/100*(last-start)/12)^-1
  return(valorcontrato)
}
Valorcontrato_liq_pago_FRA(Tasafmonth(tna,3,12),6.25,10000000,3,12)

#ii) Se puede observar que termina pagando en el prestamo una tasa fija, a pesar de que en este caso la
#liquidacion del FRA se hace en el comienzo del contrato, a los 3 meses. Si se hubiera hecho al final del
#plazo, se puede ver que te daria igual a (0,0625 - 0,0769) * 9 / 12 * 10 millones (le saco el factor de
#descuento) y a eso le restas el valor del prestamo por la tasa libor, es decir 10 millones * (1+0.00625) * 9/12 y
#te da 10 millones*(1+0,0769)=10mill*(1+tasa fija)

#b) 
Tasaswap <- function(tnas, venc, freq) {
  N = length(tnas)
  i <- 1:N
  Z <- 1 / (1 + (tnas[i]/100) * (i/12))
  
  pay <- seq(freq, venc, by = freq)
  fracc  <- 12/freq
  numer  <- 1 - Z[venc]
  denom  <- sum(Z[pay])
  TSefect = numer / denom
  TSnom = TSefect * fracc
  return(100 * TSnom)        
}
Tasaswap(tna,6,1)

#Ejercicio 4
#a)
tasa_cont <- c(6.50, 7.00, 8.00, 8.25, 8.50, 9, 9.25, 10.00, 11, 11.30, 12.00, 13.00)

Tforwmonth_cont = function(tasas, start, last){
  num= tasas[last]*last/(12*100)-tasas[start]*start/(12*100)
  denom = (last - start)/12
  tasaforw = num/denom
  return(tasaforw*100)
}

Tasacont_tna = function(tasa,periodo){
  tasa_tna= (exp(tasa*periodo/(12*100))-1)*12/periodo
  tna = tasa_tna * 100
  return(tna)
}

ValorFRA_cont_liq_cobro = function(tasafor,tasalibor,prestamo,start,last){
  valorcontrato = (tasafor-tasalibor)/100*(last-start)/12*prestamo*(1+tasalibor/100*(last-start)/12)^-1
  return(valorcontrato)
}

# forward continuo entre 9 y 12 meses (en % cont/anual)
f_c <- Tforwmonth_cont(tasa_cont, 9, 12)
# convertir ese forward continuo a forward simple anual (en %)
f_c_s <- Tasacont_tna(f_c, 3)  # ?? = 3 meses
# valor en t1 (9m) de LONG FRA = recibe fijo
V_t1_long <- ValorFRA_cont_liq_cobro(9.5, f_c_s, 1000000, 9, 12)
# precio HOY del FRA que RECIBE FIJO 
P0 <- exp(-(tasa_cont[9]/100) * (9/12)) * V_t1_long

#b)
Desc_cont = function(tascont,freq,last){
  n = length(tna)
  i = seq(freq,last, by = freq)
  P = exp(-tascont[i]*i/(12*100))
  return(P) 
}

X = Desc_cont(tasa_cont,3,9)

ValorSwap0_pago_cont = function(tasadesc,freq,VN,tasafija){
  n = length(tasadesc)
  Bv = VN
  Bf = VN*tasafija*freq/12*sum(tasadesc) + VN*tasadesc[n]    
  Swap = Bv - Bf
  return(Swap)
}

ValorSwap0_pago_cont(X,3,1000000,0.1)

#c FALSO. Para valuar un FRA no se asume que el LIBOR futuro se realizara con certeza igual a la forward.
#El precio se obtiene por no arbitraje/replicacion usando solo factores de descuento, o equivalentemente fijando que si 
#K=F el valor hoy es 0. Esto no requiere ninguna hipotesis sobre el valor que tomara la tasa spot en t1.

#ejercicio 5. A quiere variable y B fijo.
#Miremos los spreads entre A y B en cada mercado
#Fijo= 6.4-5=1.4 variable = 0.6 -0.1=0.5 Fijo > variable. A es mejor en ambos, pero su ventaja es mayor en fijo.
Swapdetasa = function(TFa,TFb,TVa,TVb,Tbanco){
  TF = TFb - TFa
  TV = TVb - TVa
  if(TF > TV){
    S= (TFb + TFa -TVb - TVa ) * 1/2
    PagoA = TFa - S + Tbanco/2
    PagoB = S + TVb + Tbanco/2
  
  }else(S=0)
  return(paste("La tasa del Swap es",S,", el Pago de A es",round(PagoA,3),"+ Libor y el pago de B es",PagoB))
}
Swapdetasa(5,6.4,0.1,0.6,0.1)

#Ejercicio 3

mes  <- c(1, 3, 6, 12)                 # plazos en meses
j4   <- c(10.00, 10.50, 11.00, 12.50)  # j(4) en %
desc_tna <- function(j_pct, months, m){
  t <- months / 12
  (1 + (j_pct/100)/m)^(-m * t)
}
Z = desc_tna(j4,mes,4)
Z
desc = (1+j4/100*3/12)^(-4*mes/12) #al plazo
desc

# a) uso suma de FRAs
TForward13 = (Z[1]/Z[2]-1)/((3-1)/12)
ValorSwapFRA3 = Z[1]*(0.098-0.1)*2/12 + Z[2]*(TForward13-0.1)*2/12
ValorSwapFRA3*100

#uso Bv - Bf ya que recibe variable y paga fija
Bv = 100*(1+0.098/6)*Z[1]
Bf = 100*0.1/6*(Z[1]+Z[2])+100*Z[2]
Sw = Bv - Bf

# b)
#Busco tasa forward entre 3 y 12 meses
tforward = ((1+0.125/4)^4/(1+0.105/4)^(4*3/12)-1)*12/9
tforward





