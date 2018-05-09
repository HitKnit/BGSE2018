# Polyrbf()

Polyrbf = function(order,x) {
  
  r = abs(x)
  
  if (is.even(order)) {
    
    r^order * log(r)
    
  } else {
    
    r^order
    
  }
  
}