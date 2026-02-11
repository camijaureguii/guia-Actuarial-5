#1)
#Un modelo de scoring obtuvo los siguientes coeficientes:
Intercepto = -2.1 ; b1 = -0.8 ; b2 = -1.2 ; b3 = -0.5 ; b4 = -0.9

#Para una empresa con ratios:
WC_TA = 0.10 ; EBIT_TA = 0.05 ; S_TA = 1.2 ; ME_TL = 0.8

#a) Calcular el score.
#b) Transformarlo en probabilidad de default (PD).
#c) ¿Esta empresa sería considerada de alto riesgo o bajo riesgo? ¿Por qué?
#d) En un modelo logit, ¿qué implica que el coeficiente sea negativo?

Score = Intercepto + b1 * WC_TA + b2 * EBIT_TA + b3 * S_TA + b4 * ME_TL
Score
Prob_default = 1/(1+exp(-Score))
Prob_default

#c) La empresa tiene baja probabilidad de default, por lo que se la clasificaría como bajo riesgo de crédito 
#(su PD está muy por debajo de los umbrales típicos de alto riesgo).

#d) En un modelo logit, un coeficiente negativo significa que, cuando aumenta esa variable, disminuyen las 
#probabilidades de default. Es decir, esas variables con coeficiente negativo actúan como factores “protectores"
#Mejores valores en ese ratio se asocian con menor riesgo de incumplimiento.
------------------------------------------------------------------------------------------------------------------------

#2) Calcular la probabilidad de default asociada a los siguientes scores:
scores = c(-6, -4, -2, 0, 2, 4, 6)

#a) Completar una tabla con Score y PD.
#b) Graficar la función logística considerando 100 valores dentro del intervalo.
#c) ¿En qué rango de scores la función es más sensible a cambios pequeños?

#a)
pd = 1/(1+exp(-scores))
score_pd = data.frame(scores,pd)
score_pd

#b)
library(ggplot2)
score = seq(-6, 6, length.out = 100)
pd = 1/(1+exp(-score))
df = data.frame(score,pd)
logit = ggplot(data=df) 
logit = logit + aes(x=score,y=pd) + geom_line(linetype=1,colour="darkblue")
logit

#c) de -6 a -4: PD sube muy poco, de -4 a -2: sube un poco más, de -2 a 0 y de 0 a 2: sube muchísimo
#de 2 a 4 y de 4 a 6: vuelve a aplanarse. La función es más sensible en 0.
#Intuición probabilística
#Cuando el modelo te da:
# - PD muy baja (por ejemplo 1%) → ya estás bastante seguro de que NO defaultea. Bajar un poco más el score casi no cambia esa certeza.
# - PD muy alta (por ejemplo 99%) → ya estás bastante seguro de que SÍ defaultea. Subir un poco más el score tampoco cambia casi nada.
# - En cambio, cuando el modelo está “en duda”, alrededor de PD ≈ 50% (score ≈ 0), cualquier información nueva 
#(pequeño cambio en el score) inclina mucho la balanza hacia un lado u otro. Por eso ahí es donde la función es más sensible.
-------------------------------------------------------------------------------------------------------------------

#3) Se tienen dos empresas con los siguientes ratios: Empresa WC.TA EBIT.TA S.TA
#                                                        A    0.15   0.08    1.0
#                                                        B    0.05  -0.02    0.9

#Y un modelo de scoring con: Intercepto = -1, coef = [-0.7, -0.9, -0.6]):
#a) Calcular el score y PD de cada empresa.
#b) ¿Cuál es más riesgosa y por qué (relacionar con los ratios)?

Ax = c(0.15,0.08,1.0)
Bx = c(0.05,-0.02,0.9)
coef = c(-0.7,-0.9,-0.6)
int = -1 
scoreA = int + sum(Ax*coef)
scoreB = int + sum(Bx*coef)
APd = 1/(1+exp(-scoreA))
BPd = 1/(1+exp(-scoreB))
scoreA
APd
scoreB
BPd

#b) es más riesgosa la B ya que su probabilidad de default es mayor
-----------------------------------------------------------------------------------------------------------------

#4) Un banco fija que sólo aprobará préstamos si la probabilidad de default < 10%.
# a) Si una empresa tiene score = -2.5, ¿se aprueba o no?
# b) ¿Cuál es el score mínimo requerido para cumplir con el umbral?
# c) ¿Qué riesgo corre el banco si fija un umbral muy bajo (ej. 1%) o muy alto (ej. 30%)?

pd = 1/(1+exp(-(-2.5)))
pd # pd = 0.07585818, se aprueba
score_min = -log(1/0.1 - 1)  #despejo el score con pd=0.1
score_min

#c) Umbral muy bajo (ej. PD < 1%): El banco sólo aprobaría empresas “excelentes”.
#Riesgo de crédito: muy bajo (casi no habrá defaults). Riesgo de negocio: altísimo → se rechazan muchísimos clientes buenos, 
#se pierde volumen de préstamos, ingresos por intereses y participación de mercado. Es una política demasiado conservadora.

#Umbral muy alto (ej. PD < 30%): El banco aprueba muchas empresas de calidad mediocre o francamente mala.
#Riesgo de crédito: muy alto → aumenta fuertemente la probabilidad de incurrir en grandes pérdidas por default, necesidad de más capital, provisiones, etc.
#Riesgo de negocio: a corto plazo crece el volumen y los ingresos, pero a mediano plazo puede volverse insostenible por la morosidad.
--------------------------------------------------------------------------------------------------------------------

#5) A partir de la base de datos histórica en el archivo “datos_ejercicio scoring.xlsx”, se pide:
#a) Importar los datos y ajustar un modelo logístico utilizando la variable Default como dependiente 
# y todos los ratios financieros como variables explicativas.
#b) Estimar, para cada observación, el score y la probabilidad de default.
#c) Agregar ambos resultados a la tabla de datos y mostrar la tabla final.
#install.packages("readxl")
library(readxl)
datos <- read_excel("C:/Users/Camila/Downloads/Datos_ejercicio scoring.xlsx")
head(datos)

modelo <- glm(Default ~ `WC/TA` + `RE/TA` + `EBIT/TA` + `ME/TL` + `S/TA`,
              data = datos,
              family = binomial(link = "logit"))
fila1 <- datos[1,]
b <- coef(modelo)
score2 <- b["(Intercept)"] + b["`WC/TA`"] * fila1$`WC/TA` + b["`RE/TA`"] * fila1$`RE/TA` + b["`EBIT/TA`"] * fila1$`EBIT/TA` +
  + b["`ME/TL`"] * fila1$`ME/TL` + b["`S/TA`"] * fila1$`S/TA`

datos$Score       <- predict(modelo, type = "link")      # b^T x
datos$PD_estimada <- predict(modelo, type = "response")  # Λ(b^T x)
datos# da distinto
--------------------------------------------------------------------------------------------------------------------------

#6) A partir de la base histórica de calificaciones crediticias de bonos corporativos incluida en el archivo “datos_ejercicio_matrices.xlsx”, se pide:
#a) Construya la matriz de transiciones
#b) Probabilidad acumulada de default en 2 años para un bono “A”
#c) Si el portafolio inicial tiene 100 bonos en “A”, ¿cuántos se espera que estén en “BBB” después de dos años?
#d) Calcule la probabilidad acumulada de default en tres años de un crédito cuyo estado inicial es “AA”
#e) ¿Cuál es la probabilidad de mejorar al menos una vez de rating dentro de los próximos 2 años para un crédito inicialmente en “A”?
library(readxl)
data <- read_excel("C:/Users/Camila/Downloads/Datos_ejercicio matrices.xlsx")

estados <- c("AAA", "AA", "A", "BBB", "D")

data$Rating_Inicial <- factor(data$"Rating Inicial", levels = estados)
data$Rating_Final   <- factor(data$"Rating Final",   levels = estados)

tabla_trans <- table(data$Rating_Inicial, data$Rating_Final)
tabla_trans

P1 <- prop.table(tabla_trans, margin = 1)
P1[is.nan(P1)] <- 0
P1

#b) 
P2 = P1 %*% P1
probDef2_A = P2["A", "D"]
probDef2_A

#c) 
probBBB2_A = P2["A", "BBB"]
probBBB2_A

#d)
P3 = P2 %*% P1
probDef3_AA = P3["AA", "D"]
probDef3_AA

#e) Mejora dentro de los dos años.
probmejor2_A = P2["A", "AA"] + P2["A", "AAA"]
probmejor2_A

mejores    <- c("AA", "AAA")
no_mejores <- setdiff(estados, mejores)

# 1) mejora en el 1er año
P1_mejora_1 <- P1["A", "AA"] + P1["A", "AAA"]

# 2) no mejora en el 1er año pero sí en el 2do
P1_desde_A_a_no_mejores <- P1["A", no_mejores]
P1_no_mejores_a_mejor   <- P1[no_mejores, "AA"] + P1[no_mejores, "AAA"]

P1_mejora_2_sin_mejorar_1 <- sum(P1_desde_A_a_no_mejores * P1_no_mejores_a_mejor)

probmejor2_A <- P1_mejora_1 + P1_mejora_2_sin_mejorar_1
probmejor2_A

----------------------------------------------------------------------------------------------------------------
library(readxl)
data <- read_excel("C:/Users/Camila/Downloads/Datos_ejercicio matrices.xlsx")

estados <- c("AAA", "AA", "A", "B", "BB", "BBB", "D")

data$Rating_Inicial <- factor(data$"Rating Inicial", levels = estados)
data$Rating_Final   <- factor(data$"Rating Final",   levels = estados)

tabla_trans <- table(data$Rating_Inicial, data$Rating_Final)
tabla_trans

P1 <- prop.table(tabla_trans, margin = 1)
P1[is.nan(P1)] <- 0
P1

#b) 
P2 = P1 %*% P1
probDef2_A = P2["A", "D"]
probDef2_A

#c) 
probBBB2_A = P2["A", "BBB"]
probBBB2_A

#d)
P3 = P2 %*% P1
probDef3_AA = P3["AA", "D"]
probDef3_AA

#e)
probmejor2_A = P1["A", "AA"] + P1["A", "AAA"] + (P2["A","AA"] - P1["A","AA"]*P1["AA","AA"]) + (P2["A","AAA"] - P1["A","AAA"]*P1["AAA","AAA"])
probmejor2_A
P2


mejores    <- c("AA", "AAA")
no_mejores <- setdiff(estados, mejores)

# 1) mejora en el 1er año
P1_mejora_1 <- P1["A", "AA"] + P1["A", "AAA"]

# 2) no mejora en el 1er año pero sí en el 2do
P1_desde_A_a_no_mejores <- P1["A", no_mejores]
P1_no_mejores_a_mejor   <- P1[no_mejores, "AA"] + P1[no_mejores, "AAA"]

P1_mejora_2_sin_mejorar_1 <- sum(P1_desde_A_a_no_mejores * P1_no_mejores_a_mejor)

probmejor2_A <- P1_mejora_1 + P1_mejora_2_sin_mejorar_1
probmejor2_A

