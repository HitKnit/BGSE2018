library(shiny)
library(data.table)
library(quantmod)
library(jrvFinance) # Package to draw BS implied volatility from option price
#library(akima) # Interpolating 3D data

# -- Mega Rbind here -- # 

ticker = "AAPL"

# Get source file directory:
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)
source("./utilities/plot.multi().R")

# Source data
temp = list.files(path = sprintf("./output/%s", ticker), pattern="*.RData")

paths = sapply(temp, function(x) {
  paste("./output/AAPL/",x, sep = "")
  })

# Load all files into list:
files = list()
for (i in 1:length(paths)) {
  
  load(paths[i])
  name = gsub(".RData","",temp[i])
  files[[i]] = assign(name, out)
  rm(out)
  
}

# It will be useful to add NAs for quotes with no output from some specific query:
quotes = lapply(files, function(x) {
  
  if("data.table" %in% class(x)) {
    
    x[,unique(ID)]
    
  }
  
})

quotes = unique(do.call(c, unique(quotes))) # unique quotes drawn at least once

# Merge all files

option_master = data.table()

for (i in 1:length(files)) {
  
  if("data.table" %in% class(files[[i]])) {
    
    # Which quotes are missing?
    novals = quotes[!(quotes %in% files[[i]][,unique(ID)])]
    nas = data.table(matrix(NA,length(novals),ncol(files[[i]])))
    colnames(nas) = colnames(files[[i]])
    nas[,ID := novals]
    nas[,TimeStamp := rep(files[[i]][,mean(TimeStamp)], nrow(nas))]
    out = rbind(files[[i]], nas) 
    
    option_master = rbind(option_master, out)
    
  } else {
    
    # Given download error make all quotes NA
    nas = data.table(matrix(NA,length(quotes),ncol(option_master)))
    colnames(nas) = colnames(option_master)
    nas[,ID := novals]
    nas[,TimeStamp := rep(files[[i]][,mean(TimeStamp)], nrow(nas))]
    option_master = rbind(option_master, nas)
    
  }
  
}

option_master[,Timetomat := Timetomat/365]
option_master[, TimeStamp := format(TimeStamp, "%y-%m%-%d %H:%M")]

#--- Assign interest and calculate implied volatility ---#
# This part generates the minute-by-minute compiled data to be used in our analysis.

# Getting Treasury Bond Data (observed daily):
symbol = "^TNX"
getSymbols(symbol, 
           from = as.Date(option_master[,min(TimeStamp)], format = "%y-%m-%d %H:%M"),
           to = as.Date(option_master[,max(TimeStamp)], format = "%y-%m-%d %H:%M"))
r_daily = coredata(Cl(TNX))/100

#--- MINUTE-BY-MINUTE COMPILED DATA ---#

# Use cubic spline to interpolate daily observations:
r = spline(r_daily, method="fmm", n= length(option_master[,unique(TimeStamp)]))$y
bond = data.table(TimeStamp = option_master[,unique(TimeStamp)],
                  r = r)
# Merge:
setkey(option_master, TimeStamp)
setkey(bond,TimeStamp)
option_master = merge(option_master, bond, all.x = T)

# Get implied volatility:
option_master[,iv:=GenBSImplied(s=Spot,X=Strike,r=r,price=Last,t=Timetomat,div_yield = 0)]

# Change object name depending on ticker:
name = sprintf("option_master_%s", ticker)
assign(name, option_master)

#save(list=name, file = sprintf("./Shiny/data/%s/option_master_%s(%s).RData",ticker,ticker,Sys.Date()))
save(list=name, file = sprintf("./output/compiled/option_master_%s.RData",ticker))

#--- DAILY COMPILED DATA ---#
# This part generates daily averages from the minute-by-minute compiled data to be used in our analysis.

option_master_day = option_master[, lapply(.SD,mean, na.rm=TRUE), by = .(TimeStamp = as.Date(TimeStamp, format = "%y-%m-%d %H:%M"), ID)]

# Change object name depending on ticker:
name = sprintf("option_master_day_%s", ticker)
assign(name, option_master_day)

#save(list=name, file = sprintf("./Shiny/data/%s/option_master_day_%s(%s).RData",ticker,ticker,Sys.Date()))
save(list=name, file = sprintf("./output/compiled/option_master_day_%s.RData",ticker))
