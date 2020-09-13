
# -------------------------------------------------------------------------------------
# Código realizado por: 
# * Juan Jose Camacho   
# * Gabriela Maria Camacho
# -------------------------------------------------------------------------------------
# Clase análisis numérico mier-vier 2001
# -------------------------------------------------------------------------------------

#Instalar Pracma
library(pracma)
library(MASS)
library(signal)
library(Rmpfr)
bits = 300
# -------------------------------------------------------------------------------------
#   2.Parte 1: Evaluar en Taylor
#     Para este punto se hizo uso de la funcion Taylor en la libreria Pracma
#     para asi poder obtener el polinomio de taylor y luego esta la funcion 
#     evaluarEnTaylor() la cual evalua el polinomio en el punto dado el cual
#     es obtenido del intervalo. Se evaluo el polinomio en 10 puntos del
#     
# -------------------------------------------------------------------------------------
#Polinomio de Taylor en Sen(X)
f = function(x)
{
  return (exp(sin(x)-sin(x)^2))
  #return(sin(x))
}

polinomioCalculado = mpfr(taylor(f,0,50),bits)

help("taylor")
print(polinomioCalculado)
inicio =  mpfr(-2^(-8),bits)
final = mpfr(2^(-8),bits)

s= seq( inicio,final,0.0009 )
print(s)
for i in s{
  horner(p = polinomioCalculado,x = c(2^(-8)))
}
print(f(2^(-8)))
