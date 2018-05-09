# Laguerre()

library(Deriv)

Laguerre = function(order,x) {
  
  term = function(x) {x**order*exp(-x)}
  exp(-x/2)*exp(x)/factorial(order)*Deriv(term, nderiv = order)(x)

}
