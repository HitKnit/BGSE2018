LSM_put_multi = function (spot = 40, sigma = 0.2, Z = 1000, N = 50, strike = 40, 
                          r = 0.06, D = 0, M = 1, order = 2, basis = Polynomial, seed = 123, 
                          dimension=2, corr=0, type="max") 
{
  
  # spot: underlying's spot price at time zero
  # sigma: underlying's volatility
  # Z: number of Monte Carlo simulations
  # N: number of times option can be exercised
  # strike: strike price
  # r: risk-free rate
  # D: dividend yield
  # M: maturity
  # order: number of basis functions to be used at regression stage
  # basis: type of basis function to be used
  # seed: random seed
  # dimension: number of underlying assets
  # corr: correlation across underlying assets
  # type: type of rainbow put option - can be put on max ("max") or put on min ("min")
  
  set.seed(seed)
  
  ### --- Warnings --- ###
  
  if( length(sigma) > dimension ) {
    
    warning("dimension should be at least as big as length(sigma)")
    
  } 
  
  if( length(r) > dimension ) {
    
    warning("dimension should be at least as big as length(r)")
    
  } 
  
  ### --- Monte Carlo --- ###
  
  # --- 1. Shocks --- #
  
  if ( length(sigma) == 1 ) { # make vector if only one value supplied
    
    sigma = rep(sigma,dimension)
    
  }
  
  # list to store seperate shocks for each asset:
  shocks = list() 
  
  for (d in 1:dimension) {
    
    shocks[[d]] = matrix(NA, nrow = Z, ncol = N)
    
  }
  
  # Generate shocks:
  if((!missing(corr))) { # if correlated processes are desired
    
    for (i in 1:Z) { 
      
      noise = corrnoise(N,corr,dimension,sigma) # generates correlated shocks
      
      for (d in 1:dimension) {
        
        shocks[[d]][i,] = noise[,d]
        
      }
      
    }
    
  } else {
    
    for (i in 1:Z) { # if non-correlated processes are desired
      
      for (d in 1:dimension) {
        
        shocks[[d]][i,] = rnorm(N, sd=sigma[d])
        
      }
      
    }
    
  }
  
  # list to store seperate prices for each asset:
  prices = list() 
  
  for (d in 1:dimension) { # start of d loop - generate multiple asset paths
    
    S = matrix(NA, nrow = Z, ncol = N+1) # empty matrix for stock price
    S[,1] = spot
    
    for (i in 1:Z) { # start of Monte Carlo Z loop
      
      # For each parameter, check whether multiple supplied. If so, choose corresponding index,
      # else choose single parameter each time:
      vol = ifelse(length(sigma)==1,sigma,sigma[d])
      int = ifelse(length(r)==1,r,r[d])
      div = ifelse(length(D)==1,D,D[d])
      x = ifelse(length(spot)==1,spot,spot[d])
      
      # Simulate Geometric Brownian Motion:
      for (j in 2:(N+1)) {
        
        S[i, j] <- S[i,j-1] * exp(cumsum(((int - div) * (M/N) - 0.5 * vol**2 * (M/N)) 
                                         + ((sqrt(M/N)) * shocks[[d]][i,j-1])))
        
      } 
      
      
    } # end of Monte Carlo Z loop
    
    prices[[d]] = S[,-1] # assign to corresponding asset
    
  } # end of d loop
  
  if (type == "max") {
    S = do.call(pmax, prices) # find parallel maxima for put on max option
  } else if (type == "min") {
    S = do.call(pmin, prices) # find parallel maxima for put on max option
  }
  
  
  ### --- Approximation of conditional expectation --- ###
  
  # We set out-the-money values to NA, in order to consider only in-the-money paths. Note that
  # for out-the-money paths there is no decision to be made anyway:
  inMon <- ifelse(S < strike, S, NA) 
  C <- matrix(pmax(0, strike - S), nrow = Z, ncol = N) # immediate payoffs in each period
  S_exp <- inMon[,-N] # values of stock prices that will be used in basis function
  
  ### Basis functions
  
  # The following list contains the features/basis functions. There is one matrix for each polynomial 
  # order:
  fts = lapply(0:order, function(x) {match.fun(basis)(x,S_exp)}) 
  
  disC <- C * exp(-1 * r * (M/N)) # discounted cash flows
  J <- cbind((matrix(NA, nrow = Z, ncol = N - 1)), disC[, N]) # discounted cash flows in terminal period
  
  # ContV will be filled with conditional expectations/the continuation value in each period and path:
  ContV <- matrix(NA, nrow = Z, ncol = N - 1) 
  
  for (t in (N - 1):1) { # beginning t loop
    
    ### --- Linear approximation --- ###
    
    # The following runs the linear approximation of the conditional expectation for 
    # each period t. We regress the present value of the payoff i - that is the true
    # continuation value unknown to market agents - in period t on the previously defined 
    # basis function. Intuitively, we map from the underlying asset to the option value.
    
    olsdata = cbind.data.frame(J[, t + 1], sapply(1:length(fts), function(x) {fts[[x]][,t]})) 
    if(length(unique(olsdata[,2])) == 1) { olsdata = olsdata[,-2] }
    colnames(olsdata)[1] = "y"
    model = lm(y ~ .,data = olsdata,na.action = na.exclude) # Regression
    
    ### --- Exercise decision --- ###
    
    # Compute fitted values (i.e. the expected payoff conditional on S_t) for each path i:
    ContV[, t] = as.matrix(cbind(rep(1,nrow(olsdata)),olsdata[,-1])) %*% as.matrix(model$coefficients)
    
    # Set out the money paths to 0:
    ContV[, t] = (ifelse(is.na(ContV[, t]), 0, ContV[, t]))
    
    # If immediate payoff is higher than conditional expected payoff, 
    # - exercise: need to update true continuation value
    # else,
    # - do not exercise: need to discount previous true continuation value by one period:
    J[, t] = ifelse(C[, t] > ContV[, t], disC[, t], J[, t + 1] * exp(-1 * r * (M/N)))
    
  } # end of t loop
  
  ContV <- ifelse(is.na(ContV), 0, ContV) # set remaining NAs to zero
  # Continuation value in terminal period is of course zero:
  ContV <- cbind(ContV, (matrix(0, nrow = Z, ncol = 1))) 
  
  # Intrinsic value of option at each point in time t for trajectory i (whenever exersised it's the 
  # immediate payoff, else it's zero):
  value <- ifelse(ContV > C, 0, C) 
  
  # Find the first time option is exercised and set all other values to zero:
  valueClean <- firstValueRow(value)
  
  # Discount the immediate exercise values back to time 0:
  presValue <- matrix(NA, nrow = Z, ncol = N)
  for (i in 1:N) {
    presValue[, i] <- valueClean[, i] * exp(-1 * M/N * r * i)
  }
  
  # Average across all simulated values:
  price <- mean(rowSums(presValue))
  
  # Output:
  res <- list(price = price, spot = spot, strike = strike, sigma = sigma, Z = Z, N = N, r = r, 
              D = D, M = M)
  return(res)
  
}