#Ejercicio 1

S0_1 <- 120 ; K_1 <- 115 ; sigma_1 <- 0.25 ; r_1 <- 0.06 ; q_1 <- 0 ; T_1 <- 2/12 ; n_1 <- 50000   

B_S = function(S0,K,sigma,r,q,T){
  d1 <- (log(S0 / K) + (r - q + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  call <- S0 * exp(-q * T) * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  return(list("call"=call, "Nd1" = pnorm(d1), "Nd2" = pnorm(d2)))
}
BS_1 = B_S(S0_1,K_1,sigma_1,r_1,q_1,T_1)
Prima_contrato_1 = BS_1$call *n_1
Prima_contrato_1

# Punto b: construcción de la cartera replicante
Acciones0_1 <- n_1 * BS_1$Nd1    # nd1 = delta del call = cant. acciones que debo comprar
Acciones0_1 #al estar corta/vendida en el call, si tengo delta positivo, para mi es negativo, ergo necesito comprar más calls para hacerlo neutro
Prestamo0_1 <- Acciones0_1* S0_1 - Prima_contrato_1 # monto que debo pedir prestado, resto la prima que vendi
Prestamo0_1

#c. Nueva situación una semana después:
S1_1 <- 122 ; t_1 <- 1/48 ; T_restante_1 <- T_1 - t_1
# Nuevo Delta
d1_1 <- (log(S1_1 / K_1) + (r_1 + 0.5 * sigma_1^2) * T_restante_1) / (sigma_1 * sqrt(T_restante_1))
Nd1_1 <- pnorm(d1_1)

#Rebalanceo
Acciones1_1 <- Nd1_1 * n_1
Ajuste_acciones_1 <- (Acciones1_1 - Acciones0_1)*S1_1
Ajuste_acciones_1

Prestamo1_1 = Ajuste_acciones_1 + Prestamo0_1*exp(r_1*t_1) 
Prestamo1_1

#ahora calculo el valor de mi cartera
d2_1 = d1_1 - sigma_1*sqrt(T_1-t_1)
d2_1
Nd2_1 = pnorm(d2_1)

C_t1 = S1_1*Nd1_1 - K_1*exp(-r_1*(T_1-t_1))*Nd2_1
C_t1

Prima_t1 = C_t1*n_1   
Prima_t1

Valor_contrato_t1 = S1_1*Acciones1_1 - Prestamo1_1 - Prima_t1
Valor_contrato_t1

#d)
BS_replicacion <- function(S_path, K, sigma, r, T, n, dt_weeks) {
  n_steps <- length(S_path) - 1
  t_step <- dt_weeks   # paso temporal en años
  
  S0 <- S_path[1]
  d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  C0 <- S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  Delta0 <- pnorm(d1)
  
  prima_total <- C0 * n
  acciones <- Delta0 * n
  prestamo <- acciones * S0 - prima_total
  
  # Guardamos el valor inicial
  historial <- data.frame(
    semana = 0,
    S = S0,
    Delta = Delta0,
    Acciones = acciones,
    Prestamo = prestamo,
    Prima = prima_total,
    Compra_Venta = acciones,
    Costo_S = prestamo,
    Valor_cartera = S0 * acciones - prestamo - prima_total
  )
  
  #REBALANCEO SEMANAL
  for (i in 1:n_steps) {
    S_t <- S_path[i + 1]
    T_restante <- T - i * t_step
    
    # Nuevo Delta
    d1_t <- (log(S_t / K) + (r + 0.5 * sigma^2) * T_restante) / (sigma * sqrt(T_restante))
    Delta_t <- pnorm(d1_t)
    
    acciones_anteriores <- acciones
    prestamo_anterior <- prestamo
    
    acciones_nuevas <- Delta_t * n
    ajuste_acciones <- (acciones_nuevas - acciones) * S_t
    
    prestamo <- prestamo * exp(r * t_step) + ajuste_acciones
    
    # Calculo valor actual del call 
    d2_t <- d1_t - sigma * sqrt(T_restante)
    C_t <- S_t * pnorm(d1_t) - K * exp(-r * T_restante) * pnorm(d2_t)
    prima_total <- C_t * n
    
    # Valor de la cartera en este instante
    valor_cartera <- S_t * acciones_nuevas - prestamo - prima_total
    
    Compra_Venta_nuevo <- acciones_nuevas - acciones_anteriores
    Costo_S_nuevo <- prestamo - prestamo_anterior
    
    # Guardo y actualizo
    historial <- rbind(historial, data.frame(
      semana = i,
      S = S_t,
      Delta = Delta_t,
      Acciones = acciones_nuevas,
      Prestamo = prestamo,
      Prima = prima_total,
      Compra_Venta = Compra_Venta_nuevo,
      Costo_S = Costo_S_nuevo,
      Valor_cartera = valor_cartera
    ))
    
    acciones <- acciones_nuevas
  }
  return(historial)
}

S_path_1 <- c(120, 122, 118, 114, 111, 114, 110, 108, 113)  # evolución semanal del precio (8 semanas)
dt_weeks_1 <- 1/48  

rdo_estrategia_1 <- BS_replicacion(S_path_1, K_1, sigma_1, r_1, T_1, n_1, dt_weeks_1)
rdo_estrategia_1
Ganancia_final_1 = rdo_estrategia_1$Valor_cartera[9]*exp(-r_1*T_1)
Ganancia_final_1

#e) Simulacion de precios
simular_precios <- function(S0, mu, sigma, dt_2, n_sims,T) {
  #Vector de tiempos y pasos 
  t_vector = seq(0, T, by = dt_2)
  n_steps = length(t_vector)
  
  #Matriz vacía para precios simulados
  tabla_precios = matrix(NA, nrow = n_steps, ncol = n_sims)
  
  #Simulación Monte Carlo de precios 
  for (k in 1:n_sims) {  # bucle que repito 5000 veces
    s = rep(NA, n_steps)  # vector de precios vacíos
    s[1] = S0             # el primer precio es el inicial
    eps_2 = c(NA, rnorm(n_steps - 1, 0, 1))  # epsilons aleatorios normales
    
    for (i in 2:n_steps) {
      s[i] = s[i - 1] * exp((mu - 0.5 * sigma^2) * dt_2 + sigma * eps_2[i] * sqrt(dt_2))
    }
    
    tabla_precios[, k] = s  # guardo la trayectoria simulada
  }
  
  #Data frame final
  colnames(tabla_precios) = paste0("sim_", 1:n_sims)
  tabla_precios_df = data.frame(t = t_vector, tabla_precios)
  
  return(tabla_precios_df)
}

set.seed(123)
mu_1 = 0.18 ; dt_1 = 1/48 ; n_sims_1 = 5000
resultado_precios <- simular_precios(S0_1, mu_1, sigma_1, dt_1, n_sims_1,T_1)
resultado_precios[, 1:10] #visualizo las 10 primeras simulaciones

calcular_cartera <- function(tabla_precios_df, K, sigma, r, T, n_contratos, dt) {
  tiempos   <- tabla_precios_df$t
  n_tiempos <- length(tiempos)
  n_sims    <- ncol(tabla_precios_df) - 1    # la 1ª columna es 't'
  
  # Matriz para guardar el valor neto de la cartera por simulación
  Cartera_sim <- matrix(NA, nrow = n_tiempos, ncol = n_sims)
  
  for (j in 1:n_sims) {                          # para cada simulación
    precios <- tabla_precios_df[[j + 1]]         # +1 por la columna 't'
    
    # Inicializaciones
    Delta <- Cantidad_acciones <- Valor_acciones <- Bonos <- Prima_call <- Cartera <- numeric(n_tiempos)
    
    for (i in 1:n_tiempos) {
      t <- tiempos[i]
      S <- precios[i]
      
        d1 <- (log(S / K) + (r + 0.5 * sigma^2) * (T - t)) / (sigma * sqrt(T - t))
        d2 <- d1 - sigma * sqrt(T - t)
        Delta[i] <- pnorm(d1)
        N_d2     <- pnorm(d2)
        C_nueva  <- S * Delta[i] - K * exp(-r * (T - t)) * N_d2
      
      
      Cantidad_acciones[i] <- Delta[i] * n_contratos
      Valor_acciones[i]    <- Cantidad_acciones[i] * S
      Prima_call[i]        <- C_nueva * n_contratos
      
      if (i == 1) {
        Bonos[i] <- Valor_acciones[i] - Prima_call[i]
      } else {
        ajuste_acciones <- Cantidad_acciones[i] - Cantidad_acciones[i - 1]
        ajuste_dinero   <- ajuste_acciones * S
        Bonos[i]        <- Bonos[i - 1] * exp(r * dt) + ajuste_dinero
      }
      
      Cartera[i] <- Valor_acciones[i] - Bonos[i] - Prima_call[i]
    }
    
    Cartera_sim[, j] <- Cartera
  }
  
  Cartera_sim_df <- data.frame(t = tiempos, Cartera_sim)
  colnames(Cartera_sim_df)[-1] <- paste0("sim_", 1:n_sims)
  return(Cartera_sim_df)
}
set.seed(123)
cartera_df <- calcular_cartera(resultado_precios,K_1, sigma_1, r_1, T_1, n_sims_1, dt_1)
cartera_df[, 1:10] #visualizo las 9 primeras simulaciones

#INTERVALO DE CONFIANZA, para los valores finales de liquidacion

# Extraer la última fila (todos los valores de cartera al final para todas las simulaciones)
carteras_finales = as.numeric(cartera_df[nrow(cartera_df), -1])  # -1 para excluir la columna t

# Calcular intervalo de confianza del 99%
IC_99 = quantile(carteras_finales, probs = c(0.005, 0.995))
IC_99

#f)

prob_perdida_1 = mean(carteras_finales < 0)   #la probabilidad de perdida con esta prima que cobro por cada call es mayor al 10%

#necesito encontrar la nueva media que hace que menos del 10% de los datos sean negativos

#en una normal estandar el valor que deja el 10% de los datos en el lado negativo es este
z_10_1 = qnorm(0.10)
z_10_1   # Esto da aproximadamente -1.2816


#sabiendo esto, busco la nueva media que hace que la probabilidad de que sea negativo el valor de la cartera sea 10% aprox
#el desvio se mantiene igual

mu_actual_1 = mean(carteras_finales)
sigma_actual_1 = sd(carteras_finales)

# 3. Nueva media necesaria
mu_necesaria_1 = -z_10_1 * sigma_actual_1  #es el Var, yo quiero que x sea igual a 0 y despejo esa mu necesaria

# 4. Cuánto te falta: (esto sería la "prima extra" total a sumar)
prima_extra_total_1 = mu_necesaria_1 - mu_actual_1

# 5. Si vendiste 50.000 contratos:
prima_extra_por_call_1 = prima_extra_total_1 / 50000

# (Si tu prima original fue prima_actual)
prima_minima_1 = BS_1$call + prima_extra_por_call_1     #sumo la prima teorica que se uso para las simulaciones
prima_minima_1

#g)  Volatilidad implicita
#dada la nueva prima que calculé arriba, que sigma estoy utilizando tecanicamente en la formula de black scholes

# Vega (derivada de C respecto a sigma)
bs_vega <- function(S0, K, r, q, T, sigma) {
  d1 <- (log(S0/K) + (r - q + 0.5*sigma^2)*T) / (sigma*sqrt(T))
  S0*exp(-q*T)*dnorm(d1)*sqrt(T)
}

implied_vol_nr <- function(S0, K, r, q, T, price_target, sigma, tol, maxit) {
  sigma <- max(sigma, 1e-6)
  for (i in 1:maxit) {
    price <- B_S(S0,K,sigma,r,q,T)
    vega  <- bs_vega(S0, K, r, q, T, sigma)
    if (!is.finite(price$call) || !is.finite(vega) || vega <= 1e-12) break
    
    incr  <- (price$call - price_target) / vega
    sigma <- max(sigma - incr, 1e-8)
    
    if (abs(incr) < tol) return(sigma)
  }
  return(NA_real_)  # si no converge
}
implied_vol_nr(S0_1, K_1, r_1, q_1, T_1, prima_minima_1, sigma_1, 1e-8, 50)

#-------------------------------------------------------------------------------------------------------------------
#Ejercicio 3
S0_3 <- 75 ; K_3 <- 78 ; sigma_3 <- 0.3 ; r_3 <- 0.08 ; q_3 <- 0 ; T_3 <- 2/12 ; n_3 <- 1000   

B_S = function(S0,K,sigma,r,q,T){
  d1 <- (log(S0 / K) + (r - q + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  call <- S0 * exp(-q * T) * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  return(list("call"=call, "Nd1" = pnorm(d1), "Nd2" = pnorm(d2)))
}
BS_3 = B_S(S0_3,K_3,sigma_3,r_3,q_3,T_3)
Prima_contrato_3 = BS_3$call *n_3
Prima_contrato_3

#b)Simulo 10000 precios distintos para los cuales veo St - K, ese es el valor de la call al vencimiento
#Promedio todos esos resultados y los descuento por la tasa libre de riesgo
sim_call = function(S0,K,mu,sigma,T,r,n_sim){
  Z = rnorm(n_sim)  # genero un vector con 10000 epsilons normal estandar
  precios_finales = S0 * exp((mu - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z)     #me da 10000 precios distintos
  
  tabla_precios = data.frame(precio_final = precios_finales)
  
  #a cada precio le resto el valor de strike, para saber el valor de la call, si es negativo no lo promedio
  payoff_promedio = mean(pmax(precios_finales - K, 0))
  
  valor_call = exp(-r * T) * payoff_promedio
  return(valor_call)
}
n_sim_3 = 10000 ; mu_3 = 0.2
set.seed(123)
valor_call_3 = sim_call(S0_3,K_3,mu_3,sigma_3,T_3,r_3,n_sim_3)
valor_total_3 = valor_call_3 * n_3
valor_total_3   

# Punto c: construcción de la cartera replicante
Acciones0_3 <- n_3 * BS_3$Nd1                # acciones que debo comprar
Acciones0_3  #al estar corta/vendida en el call, si tengo delta positivo, para mi es negativo, ergo necesito comprar más calls para hacerlo neutro
Prestamo0_3 <- Acciones0_3* S0_3 - Prima_contrato_3 # monto que debo pedir prestado, resto la prima ya que vendi
Prestamo0_3

#d) Nueva situación una semana después:
S1_3 <- 72 ; t_3 <- 1/48 ; T_restante_3 <- T_3 - t_3
# Nuevo Delta
d1_3 <- (log(S1_3 / K_3) + (r_3 + 0.5 * sigma_3^2) * T_restante_3) / (sigma_3 * sqrt(T_restante_3))
Nd1_3 <- pnorm(d1_3)
Nd1_3

#Rebalanceo
Acciones1_3 <- Nd1_3 * n_3
Ajuste_acciones_3 <- (Acciones1_3 - Acciones0_3)*S1_3
Ajuste_acciones_3

Prestamo1_3 = Ajuste_acciones_3 + Prestamo0_3*exp(r_3*t_3) 
Prestamo1_3

#ahora calculo el valor de mi cartera
d2_3 = d1_3 - sigma_3*sqrt(T_3-t_3)
d2_3
Nd2_3 = pnorm(d2_3)

C_t1_3 = S1_3*Nd1_3 - K_3*exp(-r_3*(T_3-t_3))*Nd2_3
C_t1_3

Prima_t1_3 = C_t1_3*n_3   
Prima_t1_3

Valor_contrato_t1_3 = S1_3*Acciones1_3 - Prestamo1_3 - Prima_t1_3
Valor_contrato_t1_3

#e)
BS_replicacion <- function(S_path, K, sigma, r, T, n, dt_weeks) {
  n_steps <- length(S_path) - 1
  t_step <- dt_weeks   # paso temporal en años
  
  S0 <- S_path[1]
  d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  C0 <- S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  Delta0 <- pnorm(d1)
  
  prima_total <- C0 * n
  acciones <- Delta0 * n
  prestamo <- acciones * S0 - prima_total
  
  # Guardamos el valor inicial
  historial <- data.frame(
    semana = 0,
    S = S0,
    Delta = Delta0,
    Acciones = acciones,
    Prestamo = prestamo,
    Prima = prima_total,
    Valor_cartera = S0 * acciones - prestamo - prima_total
  )
  
  #REBALANCEO SEMANAL
  for (i in 1:n_steps) {
    S_t <- S_path[i + 1]
    T_restante <- T - i * t_step
    
    # Nuevo Delta
    d1_t <- (log(S_t / K) + (r + 0.5 * sigma^2) * T_restante) / (sigma * sqrt(T_restante))
    Delta_t <- pnorm(d1_t)
    
    acciones_nuevas <- Delta_t * n
    ajuste_acciones <- (acciones_nuevas - acciones) * S_t
    
    prestamo <- prestamo * exp(r * t_step) + ajuste_acciones
    
    # Calculo valor actual del call 
    d2_t <- d1_t - sigma * sqrt(T_restante)
    C_t <- S_t * pnorm(d1_t) - K * exp(-r * T_restante) * pnorm(d2_t)
    prima_total <- C_t * n
    
    # Valor de la cartera en este instante
    valor_cartera <- S_t * acciones_nuevas - prestamo - prima_total
    
    # Guardo y actualizo
    historial <- rbind(historial, data.frame(
      semana = i,
      S = S_t,
      Delta = Delta_t,
      Acciones = acciones_nuevas,
      Prestamo = prestamo,
      Prima = prima_total,
      Valor_cartera = valor_cartera
    ))
    
    acciones <- acciones_nuevas
  }
  return(historial)
}

S_path_3 <- c(75,72,70,72,74,75,77,79,80)  # evolución semanal del precio (8 semanas)
dt_weeks_3 <- 1/48  

rdo_estrategia_3 <- BS_replicacion(S_path_3, K_3, sigma_3, r_3, T_3, n_3, dt_weeks_3)
rdo_estrategia_3

#f) Simulacion de precios
simular_precios <- function(S0, mu, sigma, dt_2, n_sims) {
  #Vector de tiempos y pasos 
  t_vector = seq(0, 8/48, by = dt_2)
  n_steps = length(t_vector)
  
  #Matriz vacía para precios simulados
  tabla_precios = matrix(NA, nrow = n_steps, ncol = n_sims)
  
  #Simulación Monte Carlo de precios 
  for (k in 1:n_sims) {  # bucle que repito n veces
    s = rep(NA, n_steps)  # vector de precios vacíos
    s[1] = S0             # el primer precio es el inicial
    eps_2 = c(NA, rnorm(n_steps - 1, 0, 1))  # epsilons aleatorios normales
    
    for (i in 2:n_steps) {
      s[i] = s[i - 1] * exp((mu - 0.5 * sigma^2) * dt_2 + sigma * eps_2[i] * sqrt(dt_2))
    }
    
    tabla_precios[, k] = s  # guardo la trayectoria simulada
  }
  
  #Data frame final
  colnames(tabla_precios) = paste0("sim_", 1:n_sims)
  tabla_precios_df = data.frame(t = t_vector, tabla_precios)
  
  return(tabla_precios_df)
}

set.seed(123)
dt_3 = 1/48 ; n_sims_3 = 10000
resultado_precios_3 <- simular_precios(S0_3, mu_3, sigma_3, dt_3, n_sims_3)
resultado_precios_3[, 1:10] #visualizo las 10 primeras simulaciones

calcular_cartera_3 <- function(tabla_precios_df, K, sigma, r, T, n_contratos, dt) {
  tiempos   <- tabla_precios_df$t
  n_tiempos <- length(tiempos)
  n_sims    <- ncol(tabla_precios_df) - 1    # la 1ª columna es 't'
  
  # Matriz para guardar el valor neto de la cartera por simulación
  Cartera_sim <- matrix(NA, nrow = n_tiempos, ncol = n_sims)
  
  for (j in 1:n_sims) {                          # para cada simulación
    precios <- tabla_precios_df[[j + 1]]         # +1 por la columna 't'
    
    # Inicializaciones
    Delta <- Cantidad_acciones <- Valor_acciones <- Bonos <- Prima_call <- Cartera <- numeric(n_tiempos)
    
    for (i in 1:n_tiempos) {
      t <- tiempos[i]
      S <- precios[i]
      
      d1 <- (log(S / K) + (r + 0.5 * sigma^2) * (T - t)) / (sigma * sqrt(T - t))
      d2 <- d1 - sigma * sqrt(T - t)
      Delta[i] <- pnorm(d1)
      N_d2     <- pnorm(d2)
      C_nueva  <- S * Delta[i] - K * exp(-r * (T - t)) * N_d2
      
      
      Cantidad_acciones[i] <- Delta[i] * n_contratos
      Valor_acciones[i]    <- Cantidad_acciones[i] * S
      Prima_call[i]        <- C_nueva * n_contratos
      
      if (i == 1) {
        Bonos[i] <- Valor_acciones[i] - Prima_call[i]
      } else {
        ajuste_acciones <- Cantidad_acciones[i] - Cantidad_acciones[i - 1]
        ajuste_dinero   <- ajuste_acciones * S
        Bonos[i]        <- Bonos[i - 1] * exp(r * dt) + ajuste_dinero
      }
      
      Cartera[i] <- Valor_acciones[i] - Bonos[i] - Prima_call[i]
    }
    
    Cartera_sim[, j] <- Cartera
  }
  
  Cartera_sim_df <- data.frame(t = tiempos, Cartera_sim)
  colnames(Cartera_sim_df)[-1] <- paste0("sim_", 1:n_sims)
  return(Cartera_sim_df)
}
set.seed(123)
cartera_df_3 <- calcular_cartera_3(resultado_precios_3,K_3, sigma_3, r_3, T_3, n_3, dt_3)
cartera_df_3[, 1:10] #visualizo las 10 primeras simulaciones

#INTERVALO DE CONFIANZA, para los valores finales de liquidacion

# Extraer la última fila (todos los valores de cartera al final para todas las simulaciones)
carteras_finales_3 = as.numeric(cartera_df_3[nrow(cartera_df_3), -1])  # -1 para excluir la columna t

# Calcular intervalo de confianza del 99%
IC_99_3 = quantile(carteras_finales_3, probs = c(0.005, 0.995))
IC_99_3

#f)

prob_perdida_3 = mean(carteras_finales_3 < 0)   #la probabilidad de perdida con esta prima que cobro por cada call es mayor al 10%

#necesito encontrar la nueva media que hace que menos del 10% de los datos sean negativos

#en una normal estandar el valor que deja el 10% de los datos en el lado negativo es este
z_10_3 = qnorm(0.10)
z_10_3   # Esto da aproximadamente -1.2816

#sabiendo esto, busco la nueva media que hace que la probabilidad de que sea negativo el valor de la cartera sea 10% aprox
#el desvio se mantiene igual

mu_actual_3 = mean(carteras_finales_3)
sigma_actual_3 = sd(carteras_finales_3)

# 3. Nueva media necesaria
mu_necesaria_3 = -z_10_3 * sigma_actual_3

# 4. Cuánto te falta: (esto sería la "prima extra" total a sumar)
prima_extra_total_3 = mu_necesaria_3 - mu_actual_3

# 5. Si vendiste 1000 contratos:
prima_extra_por_call_3 = prima_extra_total_3 / 1000

# (Si tu prima original fue prima_actual)
prima_minima_3 = BS_3$call + prima_extra_por_call_3     #sumo la prima teorica que se uso para las simulaciones
prima_minima_3

#g)  Volatilidad implicita
#dada la nueva prima que calculé arriba, que sigma estoy utilizando tecanicamente en la formula de black scholes

# Vega (derivada de C respecto a sigma)
bs_vega <- function(S0, K, r, q, T, sigma) {
  d1 <- (log(S0/K) + (r - q + 0.5*sigma^2)*T) / (sigma*sqrt(T))
  S0*exp(-q*T)*dnorm(d1)*sqrt(T)
}

implied_vol_nr <- function(S0, K, r, q, T, price_target, sigma, tol, maxit) {
  sigma <- max(sigma, 1e-6)
  for (i in 1:maxit) {
    price <- B_S(S0,K,sigma,r,q,T)
    vega  <- bs_vega(S0, K, r, q, T, sigma)
    if (!is.finite(price$call) || !is.finite(vega) || vega <= 1e-12) break
    
    incr  <- (price$call - price_target) / vega
    sigma <- max(sigma - incr, 1e-8)
    
    if (abs(incr) < tol) return(sigma)
  }
  return(NA_real_)  # si no converge
}
implied_vol_nr(S0_3, K_3, r_3, q_3, T_3, prima_minima_3, sigma_3, 1e-8, 50)

#-----------------------------------------------------------------------------------------------------------
#Ejercicio PUT 

S0_4 <- 120 ; K_4 <- 118 ; sigma_4 <- 0.25 ; r_4 <- 0.06 ; q_4 <- 0 ; T_4 <- 2/12 ; n_4 <- 20000   

BS_Put = function(S0,K,sigma,r,q,T){
  d1 <- (log(S0 / K) + (r - q + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  put <- K * exp(-r * T) * pnorm(-d2) - S0 * exp(-q * T) * pnorm(-d1) 
  return(list("put"=put, "Nd1" = pnorm(-d1), "Nd2" = pnorm(-d2)))
}
BS_4 = BS_Put(S0_4,K_4,sigma_4,r_4,q_4,T_4)
Prima_contrato_4 = BS_4$put *n_4
Prima_contrato_4

# Punto b: construcción de la cartera replicante
Acciones0_4 <- n_4 * -BS_4$Nd1                # acciones que debo vender
Acciones0_4 #al estar corta/vendida en el put, si tengo delta negativo, para mi es positivo, ergo necesito venderr más puts para hacerlo neutro
Inversion0_4 <- Acciones0_4* S0_4 - Prima_contrato_4 # monto que debo invertir, resto la prima que vendi
Inversion0_4

#c. Nueva situación una semana después:
S1_4 <- 122 ; t_4 <- 1/48 ; T_restante_4 <- T_4 - t_4
# Nuevo Delta
d1_4 <- (log(S1_4 / K_4) + (r_4 + 0.5 * sigma_4^2) * T_restante_4) / (sigma_4 * sqrt(T_restante_4))
Nd1_4 <- pnorm(-d1_4)
Nd1_4

#Rebalanceo
Acciones1_4 <- -Nd1_4 * n_4
Ajuste_acciones_4 <- (Acciones1_4 - Acciones0_4)*S1_4
Ajuste_acciones_4

Inversion1_4 = Ajuste_acciones_4 + Inversion0_4*exp(r_4*t_4) #la inversion supera el prestamo que debo pedir, invierto de nuevo
Inversion1_4

#ahora calculo el valor de mi cartera
d2_4 = d1_4 - sigma_4*sqrt(T_4-t_4)
d2_4
Nd2_4 = pnorm(-d2_4)

P_t1 = K_4*exp(-r_4*(T_4-t_4))*Nd2_4 - S1_4*Nd1_4  
P_t1

Prima_t1 = P_t1*n_4   
Prima_t1

Valor_contrato_t1 = S1_4*Acciones1_4 - Inversion1_4 - Prima_t1
Valor_contrato_t1

#d)
BS_replicacion_P <- function(S_path, K, sigma, r, T, n, dt_weeks) {
  n_steps <- length(S_path) - 1
  t_step <- dt_weeks   # paso temporal en años
  
  S0 <- S_path[1]
  d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  P0 <- K * exp(-r * T) * pnorm(-d2) - S0 * pnorm(-d1)  
  Delta0 <- -pnorm(-d1)
  
  prima_total <- P0 * n
  acciones <- Delta0 * n
  inversion <- acciones * S0 - prima_total
  
  # Guardamos el valor inicial
  historial <- data.frame(
    semana = 0,
    S = S0,
    Delta = Delta0,
    Acciones = acciones,
    Inversion = inversion,
    Prima = prima_total,
    Compra_Venta = acciones,
    Costo_S = inversion,
    Valor_cartera = S0 * acciones - inversion - prima_total
  )
  
  #REBALANCEO SEMANAL
  for (i in 1:n_steps) {
    S_t <- S_path[i + 1]
    T_restante <- T - i * t_step
    
    # Nuevo Delta
    d1_t <- (log(S_t / K) + (r + 0.5 * sigma^2) * T_restante) / (sigma * sqrt(T_restante))
    Delta_t <- -pnorm(-d1_t)
    
    acciones_anteriores <- acciones
    inversion_anterior <- inversion
    
    acciones_nuevas <- Delta_t * n
    ajuste_acciones <- (acciones_nuevas - acciones) * S_t
    
    inversion <- inversion * exp(r * t_step) + ajuste_acciones
    
    # Calculo valor actual del call 
    d2_t <- d1_t - sigma * sqrt(T_restante)
    P_t <- K * exp(-r * T_restante) * pnorm(-d2_t) - S_t * pnorm(-d1_t)  
    prima_total <- P_t * n
    
    # Valor de la cartera en este instante
    valor_cartera <- S_t * acciones_nuevas - inversion - prima_total
    
    Compra_Venta_nuevo <- acciones_nuevas - acciones_anteriores
    Costo_S_nuevo <- inversion - inversion_anterior
    
    # Guardo y actualizo
    historial <- rbind(historial, data.frame(
      semana = i,
      S = S_t,
      Delta = Delta_t,
      Acciones = acciones_nuevas,
      Inversion = inversion,
      Prima = prima_total,
      Compra_Venta = Compra_Venta_nuevo,
      Costo_S = Costo_S_nuevo,
      Valor_cartera = valor_cartera
    ))
    
    acciones <- acciones_nuevas
  }
  return(historial)
}

S_path_4 <- c(120, 122, 118, 114, 111, 114, 110, 108, 113)  # evolución semanal del precio (8 semanas)
dt_weeks_4 <- 1/48  

rdo_estrategia_4 <- BS_replicacion_P(S_path_4, K_4, sigma_4, r_4, T_4, n_4, dt_weeks_4)
rdo_estrategia_4
Ganancia_final_4 = rdo_estrategia_4$Valor_cartera[9]*exp(-r_4*T_4)
Ganancia_final_4

#e) Simulacion de precios
simular_precios <- function(S0, mu, sigma, dt_2, n_sims,T) {
  #Vector de tiempos y pasos 
  t_vector = seq(0, T, by = dt_2)
  n_steps = length(t_vector)
  
  #Matriz vacía para precios simulados
  tabla_precios = matrix(NA, nrow = n_steps, ncol = n_sims)
  
  #Simulación Monte Carlo de precios 
  for (k in 1:n_sims) {  # bucle que repito 5000 veces
    s = rep(NA, n_steps)  # vector de precios vacíos
    s[1] = S0             # el primer precio es el inicial
    eps_2 = c(NA, rnorm(n_steps - 1, 0, 1))  # epsilons aleatorios normales
    
    for (i in 2:n_steps) {
      s[i] = s[i - 1] * exp((mu - 0.5 * sigma^2) * dt_2 + sigma * eps_2[i] * sqrt(dt_2))
    }
    
    tabla_precios[, k] = s  # guardo la trayectoria simulada
  }
  
  #Data frame final
  colnames(tabla_precios) = paste0("sim_", 1:n_sims)
  tabla_precios_df = data.frame(t = t_vector, tabla_precios)
  
  return(tabla_precios_df)
}

set.seed(123)
mu_4 = 0.18 ; dt_4 = 1/48 ; n_sims_4 = 5000
resultado_precios <- simular_precios(S0_4, mu_4, sigma_4, dt_4, n_sims_4,T_4)
resultado_precios[, 1:10] #visualizo las 10 primeras simulaciones

calcular_cartera <- function(tabla_precios_df, K, sigma, r, T, n_contratos, dt) {
  tiempos   <- tabla_precios_df$t
  n_tiempos <- length(tiempos)
  n_sims    <- ncol(tabla_precios_df) - 1    # la 1ª columna es 't'
  
  # Matriz para guardar el valor neto de la cartera por simulación
  Cartera_sim <- matrix(NA, nrow = n_tiempos, ncol = n_sims)
  
  for (j in 1:n_sims) {                          # para cada simulación
    precios <- tabla_precios_df[[j + 1]]         # +1 por la columna 't'
    
    # Inicializaciones
    Delta <- Cantidad_acciones <- Valor_acciones <- Bonos <- Prima_put <- Cartera <- numeric(n_tiempos)
    
    for (i in 1:n_tiempos) {
      t <- tiempos[i]
      S <- precios[i]
      
      d1 <- (log(S / K) + (r + 0.5 * sigma^2) * (T - t)) / (sigma * sqrt(T - t))
      d2 <- d1 - sigma * sqrt(T - t)
      Delta[i] <- -pnorm(-d1)
      N_d2 <- pnorm(-d2)
      N_d1 <- pnorm(-d1)
      P_nueva  <- K * exp(-r * (T - t)) * N_d2 - S * N_d1  
      
      Cantidad_acciones[i] <- Delta[i] * n_contratos
      Valor_acciones[i]    <- Cantidad_acciones[i] * S
      Prima_put[i]        <- P_nueva * n_contratos
      
      if (i == 1) {
        Bonos[i] <- Valor_acciones[i] - Prima_put[i]
      } else {
        ajuste_acciones <- Cantidad_acciones[i] - Cantidad_acciones[i - 1]
        ajuste_dinero   <- ajuste_acciones * S
        Bonos[i]        <- Bonos[i - 1] * exp(r * dt) + ajuste_dinero
      }
      
      Cartera[i] <- Valor_acciones[i] - Bonos[i] - Prima_put[i]
    }
    
    Cartera_sim[, j] <- Cartera
  }
  
  Cartera_sim_df <- data.frame(t = tiempos, Cartera_sim)
  colnames(Cartera_sim_df)[-1] <- paste0("sim_", 1:n_sims)
  return(Cartera_sim_df)
}
set.seed(123)
cartera_df <- calcular_cartera(resultado_precios,K_4, sigma_4, r_4, T_4, n_4, dt_4)
cartera_df[, 1:10] #visualizo las 10 primeras simulaciones

#INTERVALO DE CONFIANZA, para los valores finales de liquidacion

# Extraer la última fila (todos los valores de cartera al final para todas las simulaciones)
carteras_finales = as.numeric(cartera_df[nrow(cartera_df), -1])  # -1 para excluir la columna t

# Calcular intervalo de confianza del 99%
IC_99 = quantile(carteras_finales, probs = c(0.005, 0.995))
IC_99

#f)

prob_perdida_4 = mean(carteras_finales < 0)   #la probabilidad de perdida con esta prima que cobro por cada call es mayor al 10%

#necesito encontrar la nueva media que hace que menos del 10% de los datos sean negativos

#en una normal estandar el valor que deja el 10% de los datos en el lado negativo es este
z_40_4 = qnorm(0.10)
z_40_4   # Esto da aproximadamente -1.2816


#sabiendo esto, busco la nueva media que hace que la probabilidad de que sea negativo el valor de la cartera sea 10% aprox
#el desvio se mantiene igual

mu_actual_4 = mean(carteras_finales)
sigma_actual_4 = sd(carteras_finales)

# 3. Nueva media necesaria
mu_necesaria_4 = -z_40_4 * sigma_actual_4

# 4. Cuánto te falta: (esto sería la "prima extra" total a sumar)
prima_extra_total_4 = mu_necesaria_4 - mu_actual_4

# 5. Si vendiste 20.000 contratos:
prima_extra_por_put_4 = prima_extra_total_4 / 20000

# (Si tu prima original fue prima_actual)
prima_minima_4 = BS_4$put + prima_extra_por_put_4     #sumo la prima teorica que se uso para las simulaciones
prima_minima_4

#g)  Volatilidad implicita
#dada la nueva prima que calculé arriba, que sigma estoy utilizando tecanicamente en la formula de black scholes

# Vega (derivada de C respecto a sigma)
bs_vega <- function(S0, K, r, q, T, sigma) {
  d1 <- (log(S0/K) + (r - q + 0.5*sigma^2)*T) / (sigma*sqrt(T))
  S0*exp(-q*T)*dnorm(d1)*sqrt(T)
}

implied_vol_nr <- function(S0, K, r, q, T, price_target, sigma, tol, maxit) {
  sigma <- max(sigma, 1e-6)
  for (i in 1:maxit) {
    price <- BS_Put(S0,K,sigma,r,q,T)
    vega  <- bs_vega(S0, K, r, q, T, sigma)
    if (!is.finite(price$put) || !is.finite(vega) || vega <= 1e-12) break
    
    incr  <- (price$put - price_target) / vega
    sigma <- max(sigma - incr, 1e-8)
    
    if (abs(incr) < tol) return(sigma)
  }
  return(NA_real_)  # si no converge
}
implied_vol_nr(S0_4, K_4, r_4, q_4, T_4, prima_minima_4, sigma_4, 1e-8, 50)
