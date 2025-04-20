#. Definir una función en R que, dada una variable aleatoria X con RX = (x1,..., xn) y PX (xi) = pi para 1 ≤ i ≤ n, y un
#número real t, calcule FX (t). Es decir, que calcule la probabilidad de que de la variable aleatoria discreta que toma valores
#x1,..., xn con probabilidades p1,..., pn respectivamente, sea menor o igual que t (fn de probabilidad acumulada). Probarla con la variable aleatoria del
#ejercicio 1.

#Datos del ejercicio 1: Sea X una v.a. tal que RX = {1,2,3} con la siguiente distribución de probabilidad
#P(X = 1) = 0.2 P(X = 2) = 0.5 P(X = 3) = 0.3

FX <- function(t, valores, probabilidades) {
  if (length(valores) != length(probabilidades)) {
    stop("Los vectores de valores y probabilidades deben tener la misma longitud.")
  }
  
  if (abs(sum(probabilidades) - 1) > 1e-6) {
    warning("Las probabilidades no suman 1. Verificá los datos.")
  }
  
  suma <- sum(probabilidades[valores <= t])
  return(suma)
}

valores <- c(1, 2, 3)
probabilidades <- c(0.2, 0.5, 0.3)

FX(0, valores, probabilidades)   #  0
FX(1, valores, probabilidades)   #  0.2
FX(2, valores, probabilidades)   #  0.7
FX(3, valores, probabilidades)   #  1
FX(4, valores, probabilidades)   #  1
