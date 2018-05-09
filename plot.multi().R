# Multiple series plot

plot.multi = function(matrix,x,type="l",byrow = F, ylab="values",
                      xlab = "index", col = rainbow(ncol(X)),
                      ylim = c(min(na.exclude(X)),max(na.exclude(X))),
                      main = NA) {
  
  X = matrix

  if (byrow) {
    
    if(missing(x)) {x = 1:ncol(X)}
    
    col = rainbow(nrow(X))
    plot(x = x,y = X[1,],t=type, ylim=ylim,
         ylab = ylab,
         xlab = xlab, 
         main = main)
    
    for (i in 1:nrow(X)) {
      
      points(x = x, y = X[i,],t=type,col = col[i])
      
    }
    
  } else {
    
    if(missing(x)) {x = 1:nrow(X)}
    
    plot(x = x,y = X[,1],t=type, ylim=ylim,
         ylab = ylab,
         xlab = xlab, 
         main = main)
    
    cols = rainbow(ncol(X))
    
    for (i in 1:ncol(X)) {
      
      points(x = x, y = X[,i],t=type,col = col[i])
      
    }
    
  }

}