

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

## version avec des IF et else 
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

## version avec switch 



#' Title
#'Permet de comparer les résultats de deux fonctions qui calculent les factorielles pour un entier strictement positif n.
#'
#'
#' @param n 
#' @param type 
#' soit "for" soit "while"
#'s
#' @return
#' le nombre n élevé à la factorielle (ie n!)
#' @export
#'
#' @examples 
#' compareFact(5, type = "for)
#' 
#' >[120]
compareFact <- function(n, type = "for") {
  # Vérification : n doit être entier strictement positif
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("n doit être un entier strictement supérieur à 0")
  }
  else if (n == 0) {
    res <- 1
  }
  
  else {
    # Choix de la méthode
    res <- switch(type,
                  "for"   = factFor(n),
                  "while" = factWhile(n),
                  stop("type doit être 'for' ou 'while'"))
  }
  
  return(res)
}

devtools::document()

# Benchmark

library(microbenchmark)

microbenchmark(compareFact(100,type="for"),compareFact(100,type="while"), factorial(100))

# Apply family 


# import des datas ! 

data <- read.table("https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv", sep=",", header = TRUE)
data
summary(data)

num_col = sapply(data, is.numeric)

data_num = data[,num_col]

mean_vector = apply(data_num, FUN= mean, MARGIN = 2)  
mean_vector


data_num$arr_time[1:10] <- NA

mean_vector = apply(na.exclude(data_num), FUN= mean, MARGIN = 2)  
mean_vector

min_max <- function(x){
  min = min(x)
  max = max(x)
  return(c(min,max))
}
  
min_max_vector = apply(na.exclude(data_num), FUN= min_max, MARGIN = 2) 
min_max_vector

min_max_vector = sapply(na.exclude(data_num), FUN =min_max )  
  

# itinéraire 
summary(data)
library(tidyverse)

data <- data %>% mutate(itinerary = paste(origin, dest, sep = "_"))

itinary_stats <- data %>% group_by(itinerary) %>% summarise( mean = mean(air_time , na.rm = TRUE), 
                                                            sd =  sd(air_time , na.rm = TRUE),
                                                            itinerary = itinerary ) 

itinary_stats <- itinary_stats %>% mutate(coef = mean/sd)

itinary_stats

max_coef <- sort_by(itinary_stats,itinary_stats$coef )
max_coef
max_coef[217,]


max(itinary_stats$sd, na.rm = TRUE)
                   