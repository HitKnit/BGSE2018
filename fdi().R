######################################################################
## Implicit Finite Difference Method                                ##
######################################################################

fdi <- function(s, k, r, t, sd, n = ceiling(1e3*t), m = 2*ceiling(sqrt(3*n)),
                type = c("call", "put"), style = c("european", "american"),
                grid = FALSE) {
  if (t <= 0) stop("t = ", t, " is nonpositive!")
  if (!is.wholenumber(n) || n <= 0) stop("n = ",n," is not a positive integer!")
  if (!is.wholenumber(m) || m <= 0) stop("m = ",m," is not a positive integer!")
  type <- match.arg(type); style <- match.arg(style)
  
  dt <- t / n
  m <- m + m%%2                         # Ensure m is even.
  ## FIXME: s.max depends on k, t, sd
  s.lim <- c(max=2*s, min=0)
  ds <- unname(s.lim['max'] - s.lim['min']) / m
  s.seq <- s.lim['min'] + 0:m*ds        # vector, m+1 elements
  
  f <- matrix(rep(NA, (n+1)*(m+1)), nrow=n+1)
  g2m <- function(i)  i + 1             # grid index to matrix index
  f[g2m(n),] = switch(type, call = pmax(s.seq - k, 0), put = pmax(k - s.seq, 0))
  f[,g2m(m)] = switch(type, call = s.seq[g2m(m)] - k,  put = 0)
  f[,g2m(0)] = switch(type, call = 0,                  put = k)
  
  for (i in g2m((n-1):0)) {             # Iterate from end to beginning.
    A <- matrix(0, nrow=m-1, ncol=m-1)
    B <- f[i+1,g2m(1:(m-1))]
    for (j in 1:(m-1)) {
      a <- 1/2*r*j*dt - 1/2*sd^2*j^2*dt
      b <- 1 + sd^2*j^2*dt + r*dt
      c <- -1/2*r*j*dt - 1/2*sd^2*j^2*dt
      if (j <= 1) {                     # j == 1
        A[j,1:2] <- c(b, c)
        B[1] <- B[1] - a*f[i,g2m(0)]
      }
      else if (j < m-1)
        A[j,(j-1):(j+1)] <- c(a, b, c)
      else {                            # j == m-1
        A[j,(m-2):(m-1)] <- c(a, b)
        B[m-1] <- B[m-1] - c*f[i,g2m(m)]
      }
    }
    f[i,g2m(1:(m-1))] <- solve(A, B)
    
    if (type == 'put' && style == 'american')
      f[i,] <- pmax(f[i,], k - s.seq)
  }
  
  if (grid) return(f) else return(f[g2m(0), g2m(m/2)])
}