# Multiple series plot

plot.multi = function(matrix,type="l",byrow = F) {
  
  X = matrix

  if (byrow) {
    
    cols = rainbow(nrow(X))
    plot(X[1,],t=type, ylim=c(min(X),max(X)))
    
    for (i in 1:nrow(X)) {
      
      points(X[i,],t="l",col = cols[i])
      
    }
    
  } else {
    
    plot(X[,1],t=type, ylim=c(min(X),max(X)))
    
    cols = rainbow(ncol(X))
    
    for (i in 1:ncol(X)) {
      
      points(X[,i],t="l",col = cols[i])
      
    }
    
  }

}