

facto <- 1
for (i in 1:5) {
  facto = i*facto
}
facto


facto <- 1
k <-1 
while (k<=5) {
  facto = k * facto;
  k <- k+ 1
} 
facto 


factFor <- function(n) {
  facto <- 1
  for (i in 1:n) {
    facto = i * facto
  }
  return(facto)
}

factWhile <- function(n) {
  k <- 1
  facto <- 1
  while (k <= n) {
    facto = k * facto
    
    k <- k + 1
  }
  return(facto)
}
  

facto <- factFor(5)
facto
  
facto <- factWhile(1000)
facto  
  
  
  
  
  