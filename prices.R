#----- Recording Price Data -----#

library(quantmod)
library(timeDate)
library(data.table)

# Set working directory

setwd("/Users/JacobGrapendal/Documents/BGSE/Thesis/code")

# Set to London Times:
Sys.setenv(TZ='US/Eastern')

# Jacob's optionQuery() function

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

# Today's opening and closing

dts = Sys.Date()
tms = "09:30:00"
open = timeDate(paste(dts, tms), format = "%Y-%m-%d %H:%M:%S")

tms = "16:00:00"
close = timeDate(paste(dts, tms), format = "%Y-%m-%d %H:%M:%S")

while(format(Sys.time(), "%H:%M:%S") > format(open, "%H:%M:%S") & 
      format(Sys.time(), "%H:%M:%S") < format(close, "%H:%M:%S")) {
  
  # ----- Option: ----- #
  out = NULL
  
  tin = Sys.time() # time in
  
  while (!("data.table" %in% class(out))) {
  # while condition captures cases where query fails
      
    out = try(optionQuery())
    
  }
  
  # Save seperate ones
  save(out, file = sprintf("./output/AAPL/data(%s).RData",format(Sys.time(), "[%d-%m %H:%M:%S]")))
  
  # ----- Bond rate: ----- #
  bond = data.table(getQuote("^TNX"))
  save(bond, file = sprintf("./output/bond_AAPL/bond(%s).RData",format(Sys.time(), "[%d-%m %H:%M:%S]")))
  
  tps = as.numeric(Sys.time() - tin) # time passed
  
  # snapshot every 60
  Sys.sleep(60 - max(tps,0))   
  
} 



quit(save='no')



