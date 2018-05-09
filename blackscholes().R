# Black Scholes - European 

# Computing value of plain Vanilla European put and call

blackscholes = function(spot,strike,interest,sigma,timetomat,type="call") {
  
  # spot: underlying's spot price
  # strike: strike price K
  # interest: risk-free interest rate
  # sigma: volatility of underlying
  # timetomate: time to maturity
  # type: default is "call"
  
  d1 = -1*((log(strike/spot)-(interest+sigma^2/2)*timetomat)/(sigma*sqrt(timetomat)))
  d2 = -1*((log(strike/spot)-(interest-sigma^2/2)*timetomat)/(sigma*sqrt(timetomat)))
  
  if (type == "call") {
    
    call = spot*pnorm(d1) - exp((-interest)*timetomat)*strike*pnorm(d2)
    return(call)
    
  } else {
    
    put = -spot*pnorm(-d1) + exp((-interest)*timetomat)*strike*pnorm(-d2)
    return(put)
    
  }
  
}

