LSM_put = function (spot = 40, sigma = 0.2, Z = 1000, N = 50, strike = 40, 
                    r = 0.06, D = 0, M = 1, order = 2, basis = Laguerre, seed = 123, dimension=1) 
{
  
  set.seed(seed)
  
  ### --- Monte Carlo --- ###
  
  prices = list() 
  
  for (d in 1:dimension) { # generate multiple asset paths
    
    S <- matrix(NA, nrow = Z, ncol = N) # empty matrix for stock price
    
    for (i in 1:Z) {
      
      # For each parameter, check whether multiple supplied. If so, choose corresponding index,
      # else choose single parameter each time:
      vol = ifelse(length(sigma)==1,sigma,sigma[d])
      int = ifelse(length(r)==1,r,r[d])
      div = ifelse(length(D)==1,D,D[d])
      x = ifelse(length(spot)==1,spot,spot[d])
      
      S[i, ] <- x * exp(cumsum(((int - div) * (M/N) - 0.5 * vol**2 * (M/N)) 
                                  + (vol * (sqrt(M/N)) * rnorm(N, mean = 0, sd = 1))))
      
    } 
    
    prices[[d]] = S
    
  }
  
  S = do.call(pmax, prices) # find parallel maxima
  
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
  
  y1 <- C * exp(-1 * r * (M/N)) # discounted cash flows
  y2 <- cbind((matrix(NA, nrow = Z, ncol = N - 1)), y1[, N]) # discounted cash flows in terminal period
  
  # ContV will be filled with conditional expectations/the continuation value in each period and path:
  ContV <- matrix(NA, nrow = Z, ncol = N - 1) 
  
  for (t in (N - 1):1) { # beginning t loop
    
    ### --- Linear approximation --- ###
    
    # The following runs the linear approximation of the conditional expectation for 
    # each period t. We regress the present value of the payoff i - that is the true
    # continuation value unknown to market agents - in period t on the previously defined 
    # basis function. Intuitively, we map from the underlying asset to the option value.
    
    olsdata = cbind.data.frame(y2[, t + 1], sapply(1:length(fts), function(x) {fts[[x]][,t]})) 
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
    y2[, t] = ifelse(C[, t] > ContV[, t], y1[, t], y2[, t + 1] * exp(-1 * r * (M/N)))
    
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