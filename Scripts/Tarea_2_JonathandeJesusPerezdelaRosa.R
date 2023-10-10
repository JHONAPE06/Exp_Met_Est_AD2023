#JONATHAN DE JESUS PEREZ DE LA ROSA
# 20/10/2022
# Tarea 2

# Ejercicio 2 -------------------------------------------------------------

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
81.94, 80.41, 77.7)
# Determinar el numero de observaciones
n <- length(costal)
n
# Determinar la media
costa.media <- mean(costal)
costa.media
# Desviacion estandar
costa.sd <- sd(costal)
costa.sd
# Media observada
costa.se <- costa.sd / sqrt(n)
costa.se
costa.T <- (costa.media - 80)/ costa.se
costa.T
pt(costa.T, df = n-1)

## ¿Cuál es el valor de p?
  0.01132175
## ¿Cuántos grados de libertad tiene el experimento?
  -2.364419
##¿Cuál es la hipótesis aceptada?
## La hipótesis alternativa se declara como: La media observada es menor a la declarada en los cotales de 80 kg
## ¿Existe evidencia de que el valor medio promedio de los costales observados es menor (sinificativamente)a los que anuncia el producto?
  si


# Ejercicio 2 -------------------------------------------------------------

azufre <-c(15.8, 22.7, 26.8, 19.1, 18.5, 14.4, 8.3, 25.9, 26.4, 9.8,
 22.7, 15.2, 23.0, 29.6, 21.9, 10.5, 17.3, 6.2, 18.0, 22.9,
 24.6, 19.4, 12.3, 15.9, 11.2, 14.7, 20.5, 26.6, 20.1, 17.0,
 22.3, 27.5, 23.9, 17.5, 11.0, 20.4, 16.2, 20.8, 13.3, 18.1)
# Determinar el numero de observaciones
n <- length(azufre)
n
# Determinar la media
costa.media <- mean(azufre)
costa.media
# Desviacion estandar
costa.sd <- sd(azufre)
costa.sd
# Media observada
costa.se <- costa.sd / sqrt(n)
costa.se
costa.T <- (costa.media - 17.5)/ costa.se
costa.T
pt(costa.T, df = n-1)

## ¿Cuál es el valor de p?
  1.335829
## ¿Cuáles son los intervalos de confianza al 95 % ?
  0.9039329
## ¿Cuántos grados de libertad tiene el experimento?
  0.9039329
## ¿Cuál es la hipótesis aceptada?
  
## ¿Existe evidencia de que el valor medio promedio de las emisiones observadas es mayor (significativamente) a la declarada en los procedimientos de seguridad de la empresa?
  si 


# Ejercicio 3 -------------------------------------------------------------


