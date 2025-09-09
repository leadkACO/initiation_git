

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
  

system.time({
  x <- factFor(1000)
  x
})


# comparer les résultats 

compareFact(n, type="for"){
  if(type=="for"){
    res = factFor(n)
  }
  else if(type == "while"){
    res = factWhile(n)
  }
  else{
    stop("type must be 'for' or 'while'")
  }
}


# import des datas ! 

data <- read.table("https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv", sep=",")

data
  