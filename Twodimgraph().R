
Twodimgraph = function (spot1 = 40,
                    sigma1 = 0.4,
                    spot2 = 40,
                    sigma2 = 0.7,
                    seed = 124,
                    N = 1000,
                    strike = 40,
                    r = 0.06,
                    D = 0,
                    M = 1,
                    col1='blue',
                    col2='orange',
                    col3='firebrick',
                    main='Spot Price for option with two underlying assets'){

  set.seed(seed)
  
  S1 = rep(NA, N) # empty matrix for stock price 1
  S2 = rep(NA, N) # empty matrix for stock price 2
  
  ### --- 2-Asset Monte Carlo --- ###
  
  S1 = spot1 * exp(cumsum(((r) * (M/N) - 0.5 * sigma1**2 * (M/N)) 
                                + (sigma1 * (sqrt(M/N)) * rnorm(N, mean = 0, sd = 1))))
    
  S2 = spot2 * exp(cumsum(((r) * (M/N) - 0.5 * sigma2**2 * (M/N)) 
                                + (sigma2 * (sqrt(M/N)) * rnorm(N, mean = 0, sd = 1))))
    
  S = rep(NA, N)
  
  for (i in 1:N) {
    S[i] = max(S1[i], S2[i])
  }
  
  plot(S1, type='l', col=col1, 
       ylim=c(min(S1, S2), max(S1, S2)+3),
       xlab='Time',
       ylab='Stock Price',
       main=main,
       cex.main=0.8,
       cex.lab=0.8,
       cex.axis=0.8)
  
  points(S2, col=col2, t='l')
  points(S+0.2, t='l', col=col3)
  legend('topright', legend=c("Stock 1", "Stock 2", "Option Spot Price"),
         col=c(col1, col2, col3), lty=1:1:1, cex=0.7)
}
