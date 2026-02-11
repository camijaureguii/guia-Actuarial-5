library(quantmod)
library(tidyverse)
library(ggplot2)

getSymbols(c("TSLA", "NVDA"), from = "2023-07-01", to = "2024-06-30")


data <- tibble( fecha = as.Date(index(TSLA)),
  Tesla = as.double(TSLA$TSLA.Adjusted),
  Nvda = as.double(NVDA$NVDA.Adjusted)) %>%
  mutate(  tsla_return = c(NA, log(Tesla[2:n()] / Tesla[1:(n()-1)] )),  #divido el precio de hoy por el de ayer
           nvda_return = c(NA, log(Nvda[2:n()] / Nvda[1:(n()-1)] )))    #rendimientos logaritmicos

n_tsla = 150
n_nvda = 150

#a) Determine el valor de la cartera y de la posición en cada activo. Calcule la volatilidad los activos y la 
#correlación utilizando el método “estándar” histórico. NOTA: Para anualizar los parámetros, utiliza la cantidad de 
#datos que tiene su muestra del último año.

tsla_ret <- (data$tsla_ret)
n= length(tsla_ret) #250, uso ese num

ultimo_precio_tsla <- as.numeric(tail(na.omit(data$Tesla), 1))
ultimo_precio_nvda  <- as.numeric(tail(na.omit(data$Nvda), 1))

posicion_tsla = n_tsla*ultimo_precio_tsla
posicion_nvda = n_nvda*ultimo_precio_nvda

valor_cartera = posicion_tsla + posicion_nvda
valor_cartera

#Calculo de volatilidades con metodo historico
desvio_tsla   = sd(data$tsla_return, na.rm = TRUE)
desvio_nvda   = sd(data$nvda_return, na.rm = TRUE)

vol_anual_tsla  = desvio_tsla * sqrt(250)   #250 porque es la cantidad de dias que tiene mi muestra
vol_anual_nvda  = desvio_nvda  * sqrt(250)
vol_anual_tsla
vol_anual_nvda

Rho = cor(data$tsla_return, data$nvda_return, use = "complete.obs")
#Matriz de correlaciones: la acción consigo misma tiene correlación 1

(Rho_matrix =          matrix(c(1, Rho,
                                Rho, 1), nrow = 2))

#Ahora hago la matriz de covarianzas y varianzas, diaria
matrizvarcov = matrix(c(desvio_tsla^2, desvio_tsla*desvio_nvda*Rho,
                        desvio_tsla*desvio_nvda*Rho, desvio_nvda^2), nrow = 2)

#Matriz de cov y varianzas, anual
matrizvarcov_anual = matrix(c(vol_anual_tsla^2, vol_anual_tsla*vol_anual_nvda*Rho,
                              vol_anual_tsla*vol_anual_nvda*Rho, vol_anual_nvda^2), nrow = 2)

#b) Calcule el Valor a Riesgo (VaR) individual para cada acción, asumiendo una distribución (log)normal de los retornos. 
#Realice los cálculos en términos monetarios y como porcentaje del valor de la cartera.
#hago un vector con los pesos de cada accion

#b) VaR individual
Z_99 <- qnorm(0.99)

ret_tsla <- na.omit(data$tsla_return)
vol_tsla <- vol_anual_tsla*sqrt(10/250)
VaR_rel_tsla <- Z_99 * vol_tsla
VaR_abs_tsla <- VaR_rel_tsla * posicion_tsla
VaR_porc_cartera = VaR_abs_tsla/valor_cartera

cat("Tesla: VaR absoluto (99%): $", round(VaR_abs_tsla, 2), "\n\n")
cat("Tesla: VaR relativa:", round(VaR_rel_tsla*100, 3), "%\n")
cat("Tesla: VaR como % de la cartera:", round(VaR_porc_cartera*100, 3), "%\n")

ret_nvda <- na.omit(data$nvda_return)
vol_nvda <- sd(ret_nvda)*sqrt(10) #lo mismo pero no divido sobre 250 ya que uso la diaria)
VaR_rel_nvda <- Z_99 * vol_nvda
VaR_abs_nvda <- VaR_rel_nvda * posicion_nvda
VaR_porc_cartera <- VaR_abs_nvda / valor_cartera

cat("Nvidia: VaR absoluto (99%): $", round(VaR_abs_nvda, 2), "\n\n")
cat("Nvidia: VaR relativa:", round(VaR_rel_nvda*100, 3), "%\n")
cat("Nvidia: VaR como % de la cartera:", round(VaR_porc_cartera*100, 3), "%\n")

Var_no_diver = VaR_abs_nvda + VaR_abs_tsla
Var_no_diver

#c) VaR de la cartera e indique el beneficio de diversificación.
Wi = c(posicion_tsla/valor_cartera, posicion_nvda/valor_cartera)
volatcartera = sqrt(t(Wi)%*% matrizvarcov%*% Wi) %>% as.double() 
volatcartera_anual= sqrt(t(Wi)%*% matrizvarcov_anual%*% Wi) %>% as.double()

vol_cartera = volatcartera_anual*sqrt(10/250)
VaR_rel_cartera <- Z_99 * vol_cartera
VaR_abs_cartera <- VaR_rel_cartera * valor_cartera

cat("Cartera combinada: VaR relativo (99%):", round(VaR_rel_cartera*100, 3), "%\n")
cat("Cartera combinada: VaR absoluto (99%): $", round(VaR_abs_cartera, 2), "\n")

diversificacion = VaR_abs_nvda+VaR_abs_tsla-VaR_abs_cartera
cat("Beneficio de diversificar: $", round(diversificacion, 2), "\n")

#d) VaR MArginal

Betas= (matrizvarcov_anual%*%Wi)/volatcartera_anual^2

#el primer resultado de los betas es el de TSLA y el segundo de NVDA
VaRMarginal_tsla = Betas[1,1]*vol_cartera*Z_99
VaRMarginal_tsla
VaRMArginal_nvda = Betas[2,1]*vol_cartera*Z_99
VaRMArginal_nvda

#e) VaR Component
ComponentVaR_tsla = Betas[1,1]*Wi[1]*VaR_abs_cartera
ComponentVaR_tsla
ComponentVaR_nvda = Betas[2,1]*Wi[2]*VaR_abs_cartera
ComponentVaR_nvda
Comp_porcentual_tsla = ComponentVaR_tsla/VaR_abs_cartera
Comp_porcentual_tsla
Comp_porcentual_nvda = ComponentVaR_nvda/VaR_abs_cartera
Comp_porcentual_nvda

#f) La empresa analiza comprar 100 acciones de Tesla adicionales. Calcule el VaR Incremental si se concreta esta operación.

#Calculo el var con las nuevas cantidades de acciones
n_tsla_2 = 250
n_nvda = 150

valor_cartera_2 = n_tsla_2*ultimo_precio_tsla + n_nvda*ultimo_precio_nvda

posicion_tsla_2 = n_tsla_2*ultimo_precio_tsla
posicion_nvda = n_nvda*ultimo_precio_nvda

Wi = c(posicion_tsla_2/valor_cartera_2, posicion_nvda/valor_cartera_2)

volatcartera = sqrt(t(Wi)%*% matrizvarcov%*% Wi) %>% as.double() 

volatcartera_anual= sqrt(t(Wi)%*% matrizvarcov_anual%*% Wi) %>% as.double()

vol_cartera = volatcartera_anual*sqrt(10/250)
VaR_rel_cartera <- Z_99 * vol_cartera
VaR_abs_cartera_2 <- VaR_rel_cartera * valor_cartera_2

#RESTO el var anterior con el nuevo
Var_incremental = VaR_abs_cartera_2 - VaR_abs_cartera
Var_incremental

#g) Volatilidad con EWMA
lambda= 0.94

#TSLA
tsla_return <- na.omit(data$tsla_return)
n= length(tsla_return)
aux=0
for(t in 1:n){
  aux[t]= lambda^(t-1)*tsla_return[length(tsla_return)+1-t]^2} 
volatewma_tsla= sqrt((1-lambda)*sum(aux))  #volat diaria
volatewma_anual_tsla= volatewma_tsla*sqrt(250) #volat anual

#NVDA
nvda_return <- na.omit(data$nvda_return)
n= length(nvda_return)
aux=0
for(t in 1:n){
  aux[t]= lambda^(t-1)*nvda_return[length(nvda_return)+1-t]^2}
volatewma_nvda= sqrt((1-lambda)*sum(aux))  #volat diaria
volatewma_anual_nvda= volatewma_nvda*sqrt(250) #volat anual

#Correlacion EWMA
n = (nrow(data)-1)  
aux=0

covewma<-function(R_1,R_2){
  for(t in 1:n){
    aux[t]<-lambda^(t-1)*R_1[length(R_1)+1-t]*R_2[length(R_2)+1-t] }
  covarianzaEWMA = 250*(1-lambda)/(1-lambda^n)*sum(aux)  
}

cov = covewma(tsla_return,nvda_return)
cov
RhoEWMA<-cov/(volatewma_anual_nvda*volatewma_anual_tsla) 
RhoEWMA


#H)
n <-(nrow(data)-1)
t <- 1:n
#Analizo ponderadores
ponderadores_EWMA <-(1-lambda)*lambda^(t-1)
plot(ponderadores_EWMA) # Vemos como tiende a cero
sum(ponderadores_EWMA)

EWMA_v2<-function(rendimiento){
  for(t in 1:n){
    aux[t]<-lambda^(t-1)*rendimiento[length(rendimiento)+1-t]^2}
  EWMAvolat = sqrt((1-lambda)/(1-lambda^n)*sum(aux))
}

sigma_TSLA <- EWMA_v2(tsla_return)*sqrt(252)
sigma_TSLA
sigma_nvda <- EWMA_v2(nvda_return)*sqrt(252)
sigma_nvda

RhoEWMA_Corregido<-cov/(sigma_TSLA*sigma_nvda) 
RhoEWMA_Corregido



#J) VaR Método simulación histórica,manteniendo proporciones del portafolio

data <- data %>%
  mutate(
    
    portfolio_return = (tsla_return * Wi[1]) + (nvda_return * Wi[2])
  )

h <- 10 
retornos_diarios <- na.omit(data$portfolio_return)

num_periodos <- floor(length(retornos_diarios) / h)  #redondea al mas bajo

grupos <- rep(1:num_periodos, each = h)

retornos_10_dias <- tapply(retornos_diarios[1:length(grupos)], grupos, sum)

volat_anual_10_dias <- sd(retornos_10_dias) * sqrt(250 / h)
cat(
  "Volatilidad histórica anualizada de los retornos cada 10 días:",
  round(volat_anual_10_dias * 100, 2), "%\n\n"
)


VaR_rel_hs <- -quantile(retornos_10_dias, probs = 0.01)

VaR_abs_hs <- VaR_rel_hs * valor_cartera

cat(
  "VaR de la cartera con simulación histórica (99%, 10 días):\n"
)
cat(
  "-> VaR Absoluto: $", round(VaR_abs_hs, 2), "\n"
)
cat(
  "-> VaR Relativo (como % de la cartera):", round(VaR_rel_hs * 100, 4), "%\n"
)
