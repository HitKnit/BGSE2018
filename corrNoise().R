# Correlated Mulitvariate Normal Processes

corrNoise = function(n,corr,numProc) {
  
  # n: number of values to be produced
  # avgCorr: average correlation across variables
  # numProc: number of processes
  
  # generate random correlations:
  
  mat = matrix(corr,
               numProc,
               numProc)
  diag(mat) = 1
  
  # if (!(isPositiveDefinite(mat))) {
  #   
  #   mat = makePositiveDefinite(mat)
  #   
  # }
  
  noise = mvrnorm(n=n, mu=rep(0,nrow(mat)), Sigma=mat)
  
  return(noise)
  
}


