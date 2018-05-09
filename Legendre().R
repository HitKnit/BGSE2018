# Legendre()

library(Deriv)

Legendre = function(order,x) {
  
  term = function(x) {(x**2-1)**order}
  1/(2**order*factorial(order))*Deriv(term, nderiv = order)(x)
  
}