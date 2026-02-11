#ejercicio 1
tasas_desc_efect = function(precios_bonos,VN){
  desc = precios_bonos / VN
  return(desc)
}

MA <- c(98.89, 97.75, 96.58, 95.39, 94.17, 92.93, 91.67, 90.39, 89.09, 87.77, 86.44, 85.09)
MB <- c(99.17, 98.31, 97.42, 96.50, 95.54, 94.56, 93.54, 92.50, 91.43, 90.34, 89.23, 88.09)
ZA = tasas_desc_efect(MA,100)
ZB = tasas_desc_efect(MB,100)
t <- c(1,4,7,10)
UN <- 1000
NB <- UN/3.5
S  <- 3.8
CA <- UN*0.15*(3/12)
CB <- NB*0.12*(3/12)
PV_A <- ( CA*sum(ZA[t]) + UN*ZA[10] )
PV_B <- ( CB*sum(ZB[t]) + NB*ZB[10] )
V <- S*PV_B - PV_A
V

#ejercicio 2
tasad = 0.04
tasal =0.07
cuponl = 0.1
cupond= 0.06
VNl= 20000000
VNd = 30000000
TC = 1.85
#si faltan 15 meses y el pago es anual, el prox es en 3 meses y el ultimo en 15
Cd = VNd * cupond 
Cl = VNl * cuponl
t = c(3,15)
Desc_d = Cd * sum((1+tasad)^(-t/12)) + VNd * (1+tasad)^(-15/12)
Desc_l = Cl * sum((1+tasal)^(-t/12)) + VNl * (1+tasal)^(-15/12)
#pago en dolares, recibo libras
pago_dol = TC*Desc_l - Desc_d
pago_lib = Desc_d- TC*Desc_l 
pago_lib

#ejercicio 3
Swap_moneda <- function(TAx, TAy, TBx, TBy, Tbanco) {
  # spreads (B - A) en cada moneda
  spread_usd <- TBx - TAx   
  spread_jpy <- TBy - TAy   
  if (spread_usd == spread_jpy) stop("Sin ventaja comparativa.")
  # Ventaja comparativa total a repartir
  S <- abs(spread_jpy - spread_usd)          
  S_ind <- (S - Tbanco) / 2                  
  if (spread_jpy > spread_usd) {
    A_paga_USD <- TAx - S_ind                
    B_paga_JPY <- TBy - S_ind                
    out <- paste(
      "A paga USD", A_paga_USD, "% y recibe JPY",TAy,
      "%, B paga JPY",B_paga_JPY, "% y recibe USD",TBx,
      "% y el spread del banco es",Tbanco)
  } else {
    # Caso simétrico (si la ventaja mayor fuera en USD)
    A_paga_JPY <- TAy - S_ind
    B_paga_USD <- TBx - S_ind
    out <- paste("A paga JPY",A_paga_JPY,"% y recibe USD",TAx,
      "% B paga USD",B_paga_USD,"% y recibe JPY" = TBy,
      "% y el spread del banco es",Tbanco)
  }
  return(out)
}
Swap_moneda(9.6, 5.0, 10.0, 6.5, 0.5)
