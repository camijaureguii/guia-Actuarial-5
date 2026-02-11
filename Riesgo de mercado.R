#Ejercicio 1
#a)
library(quantmod)
library(tidyverse)
#La volatilidad anualizada es cuanto varía en promedio el precio de la acción en un año

getSymbols(c("AAPL", "IBM"), from = "2024-02-01", to = "2024-4-30")

precio_AAPL <- Ad(AAPL)    #aca obtuve el precio al cierre de cada día de la acción
precio_IBM  <- Ad(IBM)
precio_AAPL
#Primero calculo los retornos diarios

data <- tibble(
  Apple = as.double(AAPL$AAPL.Adjusted),
  IBM = as.double(IBM$IBM.Adjusted)) %>%
  mutate( aapl_return = c(NA, log(Apple[2:n()] / Apple[1:(n()-1)] )), #divido el precio de hoy por el de ayer
          ibm_return = c(NA, log(IBM[2:n()] / IBM[1:(n()-1)] )))    #(Pt/Pt-1) - 1
data  

#ahora calculo los desvios en funcion de los retornos

desvio_aapl <- sd(data$aapl_return, na.rm = TRUE)

desvio_ibm <- sd(data$ibm_return, na.rm = TRUE)

#lo anualizo

vol_anual_aapl <- desvio_aapl * sqrt(252)  #para pasar de: volatilidad diaria → volatilidad anual
vol_anual_ibm  <- desvio_ibm  * sqrt(252)  #usás T=252 (días hábiles en un año bursátil)

cat("Volatilidad anualizada de Apple:", round(vol_anual_aapl * 100, 2), "%\n")
cat("Volatilidad anualizada de IBM:  ", round(vol_anual_ibm  * 100, 2), "%\n")

#también podemos calcular la volatilidad con EMWA, que implica que le doy más peso a los datos más recientes

lambda <- 0.94
returnsA <- na.omit(data$aapl_return)
nA <- length(returnsA)
var_ewmaA <- numeric(nA)
var_ewmaA[1] <- var(returnsA) # semilla inicial: puede ser la varianza muestral

for (t in 2:nA) {
  var_ewmaA[t] <- lambda * var_ewmaA[t-1] + (1 - lambda) * returnsA[t-1]^2}

# La última volatilidad EWMA (diaria, anualizada)
vol_ewma_diariaA <- sqrt(var_ewmaA[nA])
vol_ewma_anualA  <- vol_ewma_diariaA * sqrt(252)

returnsB <- na.omit(data$ibm_return)
nB <- length(returnsB)
var_ewmaB <- numeric(nB)
var_ewmaB[1] <- var(returnsB) # semilla inicial: puede ser la varianza muestral

for (t in 2:nB) {
  var_ewmaB[t] <- lambda * var_ewmaB[t-1] + (1 - lambda) * returnsB[t-1]^2}

# La última volatilidad EWMA (diaria, anualizada)
vol_ewma_diariaB <- sqrt(var_ewmaB[nB])
vol_ewma_anualB  <- vol_ewma_diariaB * sqrt(252)


cat("Volatilidad anualizada EWMA de Apple:", round(vol_ewma_anualA * 100, 2), "%\n")
cat("Volatilidad anualizada EWMA de IBM:", round(vol_ewma_anualB * 100, 2), "%\n")

#Apple tiene menos diferencia una a la otra, se puede asumir que tuvo volatilidad relativamente estable en el tiempo
#IBM en cambio tiene una gran diferencia, por lo que se puede suponer que tuvo un cambio fuerte en volatilidad 
#(por ejemplo, muy tranquila mucho tiempo y muy volátil últimamente, o al revés).


#b) CORRELACION MUESTRAL
cor(data$aapl_return, data$ibm_return, use = "complete.obs")

#CORRELACION EWMA
# Series de retornos (sin NA)
x <- na.omit(data$aapl_return)
y <- na.omit(data$ibm_return)
n <- length(x)

# Inicializar varianzas y covarianza con el cálculo muestral estándar
varx <- numeric(n); vary <- numeric(n); covxy <- numeric(n)
varx[1]  <- var(x)
vary[1]  <- var(y)
covxy[1] <- cov(x, y)

# Recurrencia EWMA
for (t in 2:n) {
  varx[t]  <- lambda * varx[t-1]  + (1 - lambda) * x[t-1]^2
  vary[t]  <- lambda * vary[t-1]  + (1 - lambda) * y[t-1]^2
  covxy[t] <- lambda * covxy[t-1] + (1 - lambda) * x[t-1] * y[t-1]
}

# Correlación EWMA más reciente:
cor_ewma <- covxy[n] / sqrt(varx[n] * vary[n])
cor_ewma

#c) Calculo VaR para una cartera: pérdida máxima esperada con 99% de confianza, en un horizonte de 5 días

# 1. Cantidad de acciones
n_aapl <- 100
n_ibm  <- 100

# 2. Últimos precios de cierre (actuales)
precio_actual_aapl <- as.numeric(tail(na.omit(data$Apple), 1))
precio_actual_ibm  <- as.numeric(tail(na.omit(data$IBM), 1))

# 3. Valores de varianza y covarianza EWMA más recientes (de tu cálculo previo)
var_aapl <- varx[n]
var_ibm  <- vary[n]
cov_ab   <- covxy[n]

# 4. Valor de la cartera hoy
valor_cartera <- n_aapl * precio_actual_aapl + n_ibm * precio_actual_ibm

# 5. Pesos en valor
w_aapl <- (n_aapl * precio_actual_aapl) / valor_cartera
w_ibm  <- (n_ibm  * precio_actual_ibm)  / valor_cartera

# 6. Varianza diaria de la cartera
var_cartera <- w_aapl^2 * var_aapl + w_ibm^2 * var_ibm + 2 * w_aapl * w_ibm * cov_ab

# 7. Volatilidad diaria
vol_cartera <- sqrt(var_cartera)

# 8. Volatilidad a 5 días (suponiendo independencia diaria, se multiplica por sqrt(5))
vol_5d <- vol_cartera * sqrt(5)

# 9. Percentil Z para 99% confianza
z_99 <- qnorm(0.99)

# 10. VaR absoluto (en valor de la cartera)
VaR_5d_99 <- valor_cartera * z_99 * vol_5d

cat("VaR al 99% para 5 días (cartera 100 IBM + 100 Apple): $", round(VaR_5d_99, 2), "\n")


#d) Ahora calculo la VaR con la metodología de simulación histórica (a partir de los datos, no suponemos distribución normal)

# 1. Precios actuales (último día disponible)
precio_aapl <- as.numeric(tail(na.omit(data$Apple), 1))
precio_ibm  <- as.numeric(tail(na.omit(data$IBM), 1))

# 2. Valor total de la cartera (100 acciones de cada una)
valor_total <- 100 * precio_aapl + 100 * precio_ibm

# 3. Proporciones actuales de la cartera
w_aapl <- (100 * precio_aapl) / valor_total
w_ibm  <- (100 * precio_ibm) / valor_total

# 4. Retornos diarios de la cartera, usando proporciones actuales
ret_cartera <- w_aapl * data$aapl_return + w_ibm * data$ibm_return
ret_cartera <- na.omit(ret_cartera)

# 5. Armar bloques de 5 días NO solapados
n <- length(ret_cartera)
n_bloques <- floor(n / 5)

# Matriz donde cada fila son 5 días consecutivos (no solapados)
bloques <- matrix(ret_cartera[1:(n_bloques*5)], ncol = 5, byrow = TRUE)

# 6. Calcular el retorno compuesto de cada bloque de 5 días
ret_5d <- apply(bloques, 1, function(x) prod(1 + x) - 1)  #multiplica cada retorno de los 5 dias (1+r1)*(1+r2)*...*(1+r5) de cada fila

# 7. Calcular el VaR al 99% (percentil 1% peor)
percentil <- 0.01
ret_5d_sorted <- sort(ret_5d)
indice_var <- ceiling(length(ret_5d_sorted) * percentil) #cada retorno tiene prob 1/12=0.083 ergo ya el primero aucumula 1% o más.
VaR_99_5d_rel <- -ret_5d_sorted[indice_var]  # pérdida positiva
VaR_99_5d_abs <- VaR_99_5d_rel * valor_total

# 8. Mostrar resultados
cat("VaR al 99% a 5 días por simulación histórica:\n")
cat("Relativo a la cartera:", round(VaR_99_5d_rel * 100, 2), "%\n")
cat("Absoluto ($):", round(VaR_99_5d_abs, 2), "\n")
cat("Tamaño de la muestra (bloques de 5 días no solapados):", length(ret_5d), "\n")
-------------------------------------------------------------------------------------------------------------

#Ejercicio 2
library(ggplot2)

#a)
getSymbols(c("AAPL", "IBM"), from = "2023-10-01", to = "2024-3-31")
precio_AAPL <- Ad(AAPL) 
precio_IBM  <- Ad(IBM)

#Primero calculo los retornos diarios
data <- tibble(
  Apple = as.double(AAPL$AAPL.Adjusted),
  IBM = as.double(IBM$IBM.Adjusted)) %>%
  mutate( aapl_return = c(NA, log(Apple[2:n()] / Apple[1:(n()-1)] )), #divido el precio de hoy por el de ayer
          ibm_return = c(NA, log(IBM[2:n()] / IBM[1:(n()-1)] )))    #(Pt/Pt-1) - 1

data$Fecha <- index(AAPL)

# Pivotear a formato largo para ggplot
data_long <- data %>%
  pivot_longer(cols = c("Apple", "IBM"), names_to = "Empresa", values_to = "Precio")

# Graficar precios diarios
ggplot(data_long, aes(x = Fecha, y = Precio, color = Empresa)) +
  geom_line(size = 1) +
  labs(
    title = "Precios de Cierre Diario: Apple vs IBM",
    x = "Fecha",
    y = "Precio de Cierre (USD)",
    color = "Empresa"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


#b) Calculo la VaR para cada accion y luego de la cartera

Z_99 <- qnorm(0.99)  # Valor crítico para 99%

# 2. Precios actuales y cantidades 
precio_aapl <- as.numeric(tail(na.omit(data$Apple), 1))
precio_ibm  <- as.numeric(tail(na.omit(data$IBM), 1))
n_aapl <- 70
n_ibm  <- 100
valor_aapl <- n_aapl * precio_aapl
valor_ibm  <- n_ibm  * precio_ibm
valor_total_cartera <- valor_aapl + valor_ibm

# 3. VaR para Apple
ret_aapl <- na.omit(data$aapl_return)
vol_aapl <- sd(ret_aapl)
VaR_rel_aapl <- Z_99 * vol_aapl *sqrt(5)  #si era anual la hubiera multiplicado por raiz(252), asumo hirozonte de 5dias
VaR_abs_aapl <- VaR_rel_aapl * valor_aapl

cat("Apple: VaR relativo (99%):", round(VaR_rel_aapl*100, 3), "%\n")
cat("Apple: VaR absoluto (99%): $", round(VaR_abs_aapl, 2), "\n\n")

# 4. VaR para IBM
ret_ibm <- na.omit(data$ibm_return)
vol_ibm <- sd(ret_ibm)
VaR_rel_ibm <- Z_99 * vol_ibm *sqrt(5)
VaR_abs_ibm <- VaR_rel_ibm * valor_ibm

cat("IBM: VaR relativo (99%):", round(VaR_rel_ibm*100, 3), "%\n")
cat("IBM: VaR absoluto (99%): $", round(VaR_abs_ibm, 2), "\n\n")

# 5. VaR para la cartera (combinada)
w_aapl <- valor_aapl / valor_total_cartera
w_ibm  <- valor_ibm  / valor_total_cartera
cor_ab <- cor(ret_aapl, ret_ibm)

# Volatilidad cartera
vol_cartera <- sqrt(
  w_aapl^2 * vol_aapl^2 +
    w_ibm^2  * vol_ibm^2  +
    2 * w_aapl * w_ibm * vol_aapl * vol_ibm * cor_ab
)

VaR_rel_cartera <- Z_99 * vol_cartera * sqrt(5) 
VaR_abs_cartera <- VaR_rel_cartera * valor_total_cartera

cat("Cartera combinada (Apple+IBM): VaR relativo (99%):", round(VaR_rel_cartera*100, 3), "%\n")
cat("Cartera combinada: VaR absoluto (99%): $", round(VaR_abs_cartera, 2), "\n")

VaR_pond_rel <- w_aapl * VaR_rel_aapl + w_ibm * VaR_rel_ibm
Beneficio_diversificacion <- VaR_pond_rel - VaR_rel_cartera
Beneficio = Beneficio_diversificacion*valor_total_cartera
Beneficio

cat("VaR ponderado (sin diversificación):", round(VaR_pond_rel*100, 3), "%\n")
cat("Beneficio absoluto por diversificación:", round(Beneficio_diversificacion*100, 3), "%\n")
cat("El VaR de la cartera es menor que el promedio ponderado de los VaR individuales por efecto de la correlación < 1.\n")

#d) 
precio_aapl <- as.numeric(tail(na.omit(data$Apple), 1))
precio_ibm  <- as.numeric(tail(na.omit(data$IBM), 1))

# 2. Valor total de la cartera (70 AAPL, 100 IBM)
valor_total <- 70 * precio_aapl + 100 * precio_ibm

# 3. Proporciones actuales
w_aapl <- (70 * precio_aapl) / valor_total
w_ibm  <- (100 * precio_ibm) / valor_total

# 4. Retornos diarios de la cartera
ret_cartera <- w_aapl * data$aapl_return + w_ibm * data$ibm_return
ret_cartera <- na.omit(ret_cartera)

# 5. Ordenar los retornos diarios de menor a mayor
ret_sorted <- sort(ret_cartera)

# 6. Calcular el VaR al 99% (percentil 1% peor)
percentil <- 0.01
indice_var <- ceiling(length(ret_sorted) * percentil)
VaR_99_1d_rel <- -ret_sorted[indice_var]  # pérdida positiva
VaR_99_1d_abs <- VaR_99_1d_rel * valor_total

# 7. Mostrar resultados
cat("VaR no paramétrico al 99% a 1 día (simulación histórica):\n")
cat("Relativo a la cartera:", round(VaR_99_1d_rel * 100, 2), "%\n")
cat("Absoluto ($):", round(VaR_99_1d_abs, 2), "\n")
cat("Tamaño de la muestra (días):", length(ret_sorted), "\n")

ret_aapl <- na.omit(data$aapl_return)
ret_sorted_aapl <- sort(ret_aapl)
indice_var_aapl <- ceiling(length(ret_sorted_aapl) * 0.01)
VaR_99_aapl_rel <- -ret_sorted_aapl[indice_var_aapl]  # positivo

# IBM
ret_ibm <- na.omit(data$ibm_return)
ret_sorted_ibm <- sort(ret_ibm)
indice_var_ibm <- ceiling(length(ret_sorted_ibm) * 0.01)
VaR_99_ibm_rel <- -ret_sorted_ibm[indice_var_ibm]

cat("VaR no paramétrico Apple (99% 1d):", VaR_99_aapl_rel, "\n")
cat("VaR no paramétrico IBM (99% 1d):", VaR_99_ibm_rel, "\n")
--------------------------------------------------------------------------------------------------------------

#Ejercicio de la filmina
# Volatilidades anuales (en proporción, no en %)
sigma <- c(A = 0.25, B = 0.15, C = 0.20)
cantidad <- c(A = 300, B = 200, C = 250)
precio <- c(A = 10, B = 40, C = 20)

#Para el Var individual:
#Busco ponderadores:
W  <- cantidad*precio
Wpond  <- W / sum(W)   # porcentajes de la cartera
z     <- qnorm(0.99)
# volatilidad 10 días de cada activo
sigma_10d_ind <- sigma * sqrt(10 / 252)

# VaR individual de cada activo
VaR_ind <- z * sigma_10d_ind * W
VaR_ind

#Para el total:
# Matriz de correlaciones (simétrica, 1 en la diagonal)
Rho <- matrix(c(1,   0.7, 0.5,
                0.7, 1,   0.6,
                0.5, 0.6, 1),
              nrow = 3, byrow = TRUE)

colnames(Rho) <- rownames(Rho) <- names(sigma)

# Matriz de varianza-covarianza
Sigma <- diag(sigma) %*% Rho %*% diag(sigma)

var_p <- as.numeric(t(Wpond) %*% Sigma %*% Wpond)  # varianza anual de la cartera
sigma_p <- sqrt(var_p)          # volatilidad anual (en proporción)
sigma_10d <- sigma_p * sqrt(10 / 252) #de la cartera
W_total = sum(W)

# c) VaR diversificado
VaR_div <- z * sigma_10d * W_total
VaR_div

#d) Busco los Betas
# Covarianza de cada activo con la cartera
cov_ip <- Sigma %*% Wpond    # vector 3x1

# Betas de cada activo
beta <- as.numeric(cov_ip) / as.numeric(var_p)
beta

# VaR por unidad de riqueza (VaR de la cartera sobre 1 dólar invertido)
VaR_por_wealth <- VaR_div / W_total

# VaR marginal de cada activo (por unidad de riqueza en ese activo)
VaR_marginal <- VaR_por_wealth * beta
VaR_marginal

# Component VaR en dólares
Component_VaR <- VaR_marginal * W

# Porcentaje del VaR total que aporta cada activo
pct_VaR <- Component_VaR / VaR_div * 100
----------------------------------------------------------------------------------------------------------
  
#Ejercicio 3
# Volatilidades anuales (en proporción, no en %)
sigma <- c(A = 0.2, B = 0.15, C = 0.22)
cantidad <- c(A = 250, B = 300, C = 150)
precio <- c(A = 15, B = 30, C = 20)

#Para el Var individual:
#Busco ponderadores:
W  <- cantidad*precio
Wpond  <- W / sum(W)   # porcentajes de la cartera
z     <- qnorm(0.99)
# volatilidad 10 días de cada activo
sigma_10d_ind <- sigma * sqrt(10 / 252)

# VaR individual de cada activo
VaR_ind <- z * sigma_10d_ind * W
VaR_ind

#Para el total:
# Matriz de correlaciones (simétrica, 1 en la diagonal)
Rho <- matrix(c(1,   0.6, 0.5,
                0.6, 1,   0.7,
                0.5, 0.7, 1),
              nrow = 3, byrow = TRUE)

colnames(Rho) <- rownames(Rho) <- names(sigma)

# Matriz de varianza-covarianza
Sigma <- diag(sigma) %*% Rho %*% diag(sigma)

var_p <- as.numeric(t(Wpond) %*% Sigma %*% Wpond)  # varianza anual de la cartera
sigma_p <- sqrt(var_p)          # volatilidad anual (en proporción)
sigma_10d <- sigma_p * sqrt(10 / 252) #de la cartera
W_total = sum(W)

# c) VaR diversificado
VaR_div <- z * sigma_10d * W_total
VaR_div

#d) Busco los Betas
# Covarianza de cada activo con la cartera
cov_ip <- Sigma %*% Wpond    # vector 3x1

# Betas de cada activo
beta <- as.numeric(cov_ip) / as.numeric(var_p)
beta

# VaR por unidad de riqueza (VaR de la cartera sobre 1 dólar invertido)
VaR_por_wealth <- VaR_div / W_total

# VaR marginal de cada activo (por unidad de riqueza en ese activo)
VaR_marginal <- VaR_por_wealth * beta
VaR_marginal

# Component VaR en dólares
Component_VaR <- VaR_marginal * W

# Porcentaje del VaR total que aporta cada activo
pct_VaR <- Component_VaR / VaR_div * 100
--------------------------------------------------------------------------------------------------------
#EJERCICIO 4
cartera = 10000000
sigma_anual = 0.25
sigm_diario = 0.0157
z99 <- qnorm(0.99)

VaR = z99*sigm_diario*cartera
VaR









