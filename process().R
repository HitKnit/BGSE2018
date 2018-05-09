# Simulation of stock prices 

process = function(spot=100,sigma=0.2,vega=0.3,theta=0.09,kappa=3,r=0,M=1,N=1000,rho=0.5,seed=1) {
  
  # spot: spot price of underlying
  # sigma: initial value of volatility
  # vega: volatility of volatility
  # theta: mean volatility
  # kappa: mean reversion parameter
  # r: interest rate
  # M: time to maturity
  # N: number of discrete time intervals
  # rho: correlation between the two BMs
  # seed: random seed
  
  set.seed(seed)
  
  s = c(spot)
  v = c(sigma ** 2)
  
  for (i in 1:N) {
    
    dW = sqrt(M / N) * rnorm(1)
    dB = sqrt(M / N) * rnorm(1)
    
    # Volatility:
    v[i+1] = ifelse(v[i] + kappa * (theta - v[i]) * (M / N) + vega * sqrt(v[i]) * dW >= 0,
                    v[i] + kappa * (theta - v[i]) * (M / N) + vega * sqrt(v[i]) * dW,
                    0)
    
    # Price:
    s[i+1] = s[i] + ( s[i] * (r * (M / N) + sqrt(v[i+1]) * (rho * dW + sqrt(1 - (rho ** 2)) * dB)) )
    
  }
  
  return(cbind(price = s, 
               var = v))
  
}