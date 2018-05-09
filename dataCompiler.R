library(shiny)
library(data.table)

# -- Mega Rbind here -- # 

ticker = "FB"

# Get source file directory:
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)
source("./utilities/plot.multi().R")

# Source data
temp = list.files(path = sprintf("./output/%s", ticker), pattern="*.RData")

paths = sapply(temp, function(x) {
  sprintf("./output/%s/%s",ticker,x)
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

# Change object name depending on ticker
name = sprintf("option_master_%s", ticker)
assign(name, option_master)

save(list=name, file = sprintf("./Shiny/data/%s/option_master_%s(%s).RData",ticker,ticker,Sys.Date()))
load(sprintf("./Shiny/data/%s/option_master_%s(%s).RData",ticker,ticker,Sys.Date()))

# X = matrix(as.matrix(option_master[,Last,by=ID][,2]), ncol=length(option_master[,unique(ID)]))
# t = matrix.POSIXct(as.POSIXct(as.matrix(option_master[,TimeStamp,by=ID][,2])), ncol=length(option_master[,unique(ID)]))
# plot.multi(X, t, bg="black", fg="white", col.axis = "white", col.lab = "white")


