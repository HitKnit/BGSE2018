optionQuery = function(ticker='AAPL', type="calls") {
  
  spot=getQuote(ticker)$Last
  opts=getOptionChain(ticker, Exp=c("2018-06-01", "2020-06-19"))
  
  if (type == 'calls') {
    
    for (i in 1:length(opts)) {
      opts[[i]]=opts[[i]]$calls
      opts[[i]]$Timetomat=as.POSIXct(names(opts)[i], format = "%b.%d.%Y")-Sys.time()
      opts[[i]]$Spot=spot
      opts[[i]]$ID=rownames(opts[[i]])
      opts[[i]]$TimeStamp = Sys.time()
      
    }
  }
  
  if (type == 'puts') {
    
    for (i in 1:length(opts)) {
      opts[[i]]=opts[[i]]$puts
      opts[[i]]$Timetomat=as.POSIXct(names(opts)[i], format = "%b.%d.%Y")-Sys.time()
      opts[[i]]$Spot=spot
      opts[[i]]$ID=rownames(opts[[i]])
      opts[[i]]$TimeStamp = Sys.time()
      
    }
  }
  
  opt_mat=rbindlist(opts)
  
  return(opt_mat)
  
}