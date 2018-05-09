### Global setup file for workspace

rm(list = ls())

### Preferences

threeD = F # 3D plotting functions?

par(mar=c(4,4,1,1),oma=c(0,0,0,0))

load <- c("foreign","stargazer","xtable","gdata","scales", # General packages
          "rdrop2", "dplyr", "car", "knitr", "bizdays", "timeDate",
          "plot3D", "RColorBrewer", "devtools", "sandwich", "zoo", 
          "tseries","moments","R.utils","gtools","forecast", "lmtest",
          "Matrix") 

# others <- c("rgl")

lapply(load, function(x) {
  if(!require(x, character.only = T)) {
    install.packages(x)
    require(x, character.only = T)
  }
})


# xtable specials

options(xtable.comment = FALSE)
xtable <- function(x, ...) {
  for (i in which(sapply(x, function(y) !all(is.na(match(c("POSIXt","Date"),class(y))))))) x[[i]] <- as.character(x[[i]])
  xtable::xtable(x, ...)
}

# 3D plotting 

# From: https://gist.githubusercontent.com/kzktmr/01c25068df145aa0b97f/raw/a0710f91fdad6698fc76aed339f98460f9944cf0/barplot3d.R

if (threeD == T) {
  binplot.3d <- function(x, y, z, alpha=1, topcol="#ff0000", sidecol="#aaaaaa", linecol="#000000")
  {
    save <- par3d(skipRedraw=TRUE)
    on.exit(par3d(save))
    x1 <- c(rep(c(x[1], x[2], x[2], x[1]), 3), rep(x[1], 4), rep(x[2], 4))
    z1 <- c(rep(0, 4), rep(c(0, 0, z, z), 4))
    y1 <- c(y[1], y[1], y[2], y[2], rep(y[1], 4), rep(y[2], 4), rep(c(y[1], y[2], y[2], y[1]), 2))
    x2 <- c(rep(c(x[1], x[1], x[2], x[2]), 2), rep(c(x[1], x[2], rep(x[1], 3), rep(x[2], 3)), 2))
    z2 <- c(rep(c(0, z), 4), rep(0, 8), rep(z, 8))
    y2 <- c(rep(y[1], 4), rep(y[2], 4), rep(c(rep(y[1], 3), rep(y[2], 3), y[1], y[2]), 2))
    rgl.quads(x1, z1, y1, col=rep(sidecol, each=4), alpha=alpha)
    rgl.quads(c(x[1], x[2], x[2], x[1]), rep(z, 4), c(y[1], y[1], y[2], y[2]), col=rep(topcol, each=4), alpha=1) 
    rgl.lines(x2, z2, y2, col=linecol, alpha = alpha)
  }
  
  barplot3d <- function(z, alpha=1, col="#ff0000", scale=1, linecol = NA) # 1 is default value for alpha and scale
  {
    save <- par3d(skipRedraw=TRUE)
    on.exit(par3d(save))
    z <- as.matrix(z)
    xy <- dim(z)
    x <- seq(xy[1])     # 1 to number of rows
    y <- seq(xy[2])     # 1 to number of cols
    z <- z / max(z, na.rm=TRUE) * max(x, y) * scale
    zcol <- seq(min(z),max(z),length.out =  length(col)) # discrete sequence of N values ranging from min(z) to max(z), where N is the number of colours in the palette
    for (i in x) 
    {
      for (j in y) 
      {
        colIndex <- which.min(abs(zcol-z[i,j]))
        binplot.3d(c(i, i+1), c(j, j+1), z[i,j], alpha=alpha, topcol=col[colIndex], sidecol = col[colIndex], linecol = linecol[colIndex])
      }
    }
  }
}

rm(threeD)