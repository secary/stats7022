calculate_mean <- function(x){
  n <- length(x)
  total <- 0
  for(i in 1:n){
    total <- total + x[i]
  }
  x_bar <- total / n
  return(x_bar)
}
calculate_mean(1:10)
