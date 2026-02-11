#Ejercicio 1
#a Valor inical =0
precioK= 40*exp(0.1) #capitalizo
precioK
#b
precioforward = 45*exp(0.1*0.5)
precioforward 
preciocontrato6_1 = (precioforward - precioK) * exp(-0.1*0.5)
preciocontrato6_1
preciocontrato6_2 = 45 - precioK*exp(-0.1*0.5)
preciocontrato6
 
#Ejercicio 2
forwardmoneda_long_y_short = function(tasaDfin,tasaPfin,tasaDinv,tasaPinv,precioC,precioV){
  #posicion larga
  prestamo_dolardesc = (1+tasaDfin)^-1
  vendo_dolar = prestamo_dolardesc * precioV
  invierto_pesos = vendo_dolar * (1+tasaPinv)
  #posicion corta
  invierto_dolar = (1+tasaDinv)^-1
  prestamo_pesosdesc = invierto_dolar * precioC
  prestamo_final = prestamo_pesosdesc * (1+tasaPfin)
  return(paste("Posicion long es",round(invierto_pesos,3),"y posicion short es",prestamo_final))
}
forwardmoneda_long_y_short(0.06,0.3,0.04,0.26,23,22.3)

#ejercicio 3
#El dividendo de $10 se paga a los 9 meses, fuera del período del forward (0–6 meses), así que su PV dentro del contrato es 0.
a_precioforw_6m = 150*exp(0.12*0.5) #no resto los 10 ya que si compro forward no los recibo
a_precioforw

#b) tomo posición vendida en el forward, compro accion hoy tomando prestamo, en 6 meses resto lo que me dan de vender el forward
#con lo que debo pagar de préstamo 
b_arbitraje_6m =(165-a_precioforw)
b_arbitraje_6m

#c) La clave es igualar los flujos de un forward: en𝑇recibís el activo y pagás K. Si vos comprás hoy el activo pidiendo un préstamo, 
#vas a cobrar el dividendo a los 9 meses. Para que esa cartera replicadora tenga los mismos flujos que el forward hay que “neutralizar” ese dividendo. 
                                                                                                                                                                                                                                                                                                                                               
c_precioforw_12m = (150-10*exp(-0.12*9/12))*exp(0.12) #o 150*exp(0.12)-10*exp(0.12*3/12)
c_precioforw_12m

#d) El de mercado está caro. Compro lo barato, vendo lo caro. Vendo el de mercado. Me obligó a vender la acción
#al vencimiento. Entonces, hoy pido un préstamo para comprarla así ya me aseguro tenerla al
#vencimiento y me cubro de los altibajos del precio de la acción en el mercado.
d_arbitraje_12m = 160 - c_precioforw_12m
d_arbitraje_12m

#ejercicio 6
preciofor_6 = 50*exp(-0.06)*exp(0.1)
preciofor_6

#ejercicio 4 
#Que el futuro tenga multiplicador 100 significa que cada punto del índice vale 100 unidades monetarias en el contrato.
#Valor nocional por contrato = precio del futuro × multiplicador.
#Para que al final tengas el mismo valor que efectivo a rf, elegimos NF igualando valores futuros
valor_portafolio <- 950000000           # $950 millones
precio_futuro <- 1564                   # Precio del contrato futuro
multiplicador <- 100                    # Tamaño del contrato
tasa_rf <- 0.0575                       # Tasa libre de riesgo (anual, continua)
tasa_div <- 0.0045                      # Tasa de dividendos (anual)
plazo <- 0.5                            # Plazo en años (6 meses)

# Cálculo del número de contratos necesarios
NF = round(valor_portafolio*(1+tasa_rf)^(6/12)/(precio_futuro*multiplicador),0)
NF

monto_efect = round((NF * 1564 * 100)/(1+tasa_rf)^0.5,1) #al redondearse la cantidad de contratos, la cantidad efectiva de dinero es menor
monto_efect

#si quiero ver cuanto va a crecer en 6 meses
Invertido_tlr = monto_efect*(1+tasa_rf)^0.5
Invertido_tlr

#si quiero ver cuántas acciones equivalen hoy:
Acciones_equiv_hoy = NF*multiplicador/(1+tasa_div)^0.5
Acciones_equiv_hoy

#si quiero ver cuántas equivalen en 6 meses
Acciones_equiv_6m = Acciones_equiv_hoy*(1+tasa_div)^0.5
Acciones_equiv_6m

#demostracion que si en 6m el indice de acciones = 1735, da lo imso que haber invertido a la tasa rf en 0:
Valoracc_6m = Acciones_equiv_6m*1735
Valoracc_6m

Liq_futuro = NF*100*(1564-1735)
Liq_futuro

Resultado_6m = Valoracc_6m + Liq_futuro
Resultado_6m #igual que en invertido_tlr

#ejercicio 5
NF_5 = 50000000*0.87/(1259*250)
NF_5

#ejercicio de indice sobre futuros
TNAr_trim = 0.08
TNAq_trim = 0.04
P= 10000
Bc =1.2
Bf = 0.95
Indice = 156
multip =25

#a)Si quisiera eliminar el riesgo de mercado de la economía B, ¿qué posición debería tomar?
#Busco valor nocional del contrato
S0 = Indice*multip #precio spot * mult
#Buscamos el valor del futuro
F0 = S0 * (1+TNAr_trim/4)^(4/3) / (1+TNAq_trim/4)^(4/3)
#las tasas son trimestrales pero los futuros vencen en 4 meses. Ahora busco la cantidad de contratos
Nf = P/F0 *(0-Bc)/Bf
Nf_red = round(abs(P/F0 *(0-Bc)/Bf),0) #vendo futuros porque debo reducir beta
#B) ¿Cómo funcionaría la cobertura si dentro de tres meses el valor del índice es 150 y el contrato de futuro cotiza a 25 · 151?
S3= 150*25
F3 =151*25
  
#Resultado por futuros a los 3 meses
Rdo_futuros = (F3-F0)*Nf #uso el Nf con coma para mostrar cómo la cobertura “ideal” elimina completamente el beta.
Rdo_futuros

#busco el resultado de la cartera original, para ello necesito el rendimiento total de la cartera
rend_cap = S3/S0-1
rend_div = TNAq_trim/4
rend_libre_riesgo = TNAr_trim/4
rend_cartera = Bc*(rend_cap+rend_div-rend_libre_riesgo) + rend_libre_riesgo
Rdo_cartera_orig = P * rend_cartera
Rend_efect_trimes = (Rdo_cartera_orig + Rdo_futuros)/P
#¿La cobertura funcionó? Queríamos eliminar el riesgo de mercado, o sea, neutralizar el efecto 
#de los movimientos del índice sobre tu cartera. Esto se traduce en: - Llevar el beta efectivo 
#a 0. - Lograr que el rendimiento final de la cartera se aproxime al de la tasa libre de riesgo, 
#que es del 2% trimestral. El rendimiento bajo nuestra estrategia fue de 1.83%, por lo tanto, 
#funciona la cobertura.

#C)¿Podría eliminar también el riesgo de tipo de cambio? ¿Por qué nominal tomaría cobertura? ¿se eliminaría totalmente?
#Mi cartera está en moneda extranjera (economía B), pero yo mido resultados en mi moneda doméstica.
#Entonces, estoy largo en la divisa extranjera → si la moneda extranjera se deprecia contra tu moneda, pierdo valor.
#Por ello, quiero cubrir la exposición a que el tipo de cambio baje.
#Si tomás un short (venta) en el futuro, gano si la divisa extranjera baja.
#Si la divisa sube, tu cartera gana (y el short en futuros pierde).
#Si la divisa baja, tu cartera pierde (y el short en futuros gana).
#Por ello, abro un contrato de futuros en el mercado con posición vendedora sobre un nominal de: 
VN_esperado = P*(1+rend_libre_riesgo)
VN_esperado
#Al vencimiento, el resultado es: Resultado=(𝐹hoy - Fvenc) × N
#Si el tipo de cambio baja, 𝐹venc < 𝐹hoy , entonces tu posición short gana, 
#compensando la pérdida de la cartera en moneda extranjera.

#Aunque esta estrategia reduce el riesgo cambiario, no lo elimina completamente, ya que: 
#- Los betas usados son estimaciones teóricas. - El rendimiento real puede diferir de la tasa 
#libre de riesgo. - El valor nominal cubierto es una aproximación esperada, no exacta.
