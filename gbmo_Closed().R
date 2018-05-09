# Geometric Brownian Motion

gbmo_Closed = function(spot,r,sigma,M,N) {
  
  # spot: spot price of underlying
  # r: interest rate
  # sigma: volatility (can be a vector or scalar)
  # M: time to maturity
  # N: number of discrete time intervals
  
  spot * exp(cumsum((r - 0.5 * sigma**2) * M/N 
                    + (sigma * (sqrt(M/N)) * rnorm(N, mean = 0, sd = 1))))
  
}
  

