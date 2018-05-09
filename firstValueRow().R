# firstValueRow

firstValueRow = function(x) {
  
  idxs = sapply(1:nrow(x), function(i) {which(x[i,]!=0)[1]})
  res = matrix(0,nrow = nrow(x),ncol = ncol(x))
  for (i in 1:nrow(x)) {
    
    res[i,idxs[i]] = x[i,idxs[i]]
    
  }
  return(res)
  
}