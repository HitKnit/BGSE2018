# Fourier()

Fourier = function(order,x) {
  
  res = sapply(x, function(x) {
    
    fx = function(x) {sin(x)+sin(2*x)}
    a0 = 1/pi * as.numeric(integrate(fx,-pi,pi)[1])
    A = sapply(1:order, function(o) {(1/pi * as.numeric(integrate(function(x) {fx(x) * cos(o * x)},-pi,pi)[1]))}) %*% sapply(1:order, function(o) {cos(x*o)})
    B = sapply(1:order, function(o) {(1/pi * as.numeric(integrate(function(x) {fx(x) * sin(o * x)},-pi,pi)[1]))}) %*% sapply(1:order, function(o) {sin(x*o)})
    
    1/2 * a0 + A + B
    
  })
  
  res = matrix(res,dim(x)[1],dim(x)[2])
  
  return(res)
  
}