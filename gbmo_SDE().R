# Geometric Brownian Motion using SDE

gbmo_SDE = function(spot,r,sigma,M,N) {
  
  # spot: spot price of underlying
  # r: interest rate
  # sigma: volatility (can be a vector or scalar)
  # M: time to maturity
  # N: number of discrete time intervals
  
  if (length(sigma) == 1) {
    
    sigma = rep(sigma,N)
    
  }
  
  s = c(spot)
  
  for (i in 2:N) {
    
    s[i] = s[i-1] *  (1 + sigma[i] * (sqrt(M/N)) * rnorm(1,0,1) + r * (M/N))
    
  }
  
  return(s)
  
}