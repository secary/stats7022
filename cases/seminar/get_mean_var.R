pacman::p_load(tidyverse)

get_mean_var <- function(x) {
  # make sure x is numeric, since we can't take the mean of characters
  stopifnot(is.numeric(x))
  
  # make sure x is a vector, since there are some data structures (like a
  # data.frame) that won't be correctly indexed by our code
  stopifnot(is.vector(x))
  
  # how many numbers in dataset
  n <- length(x)
  
  # don't do calculations of there's only 1 number
  if (n == 1) {
    return(tibble(mean = x, var = 0))
  }
  
  # set up vectors to hold means and M2s
  mean <- numeric(n)
  M2 <- numeric(n)
  
  # add first values
  mean[1] <- x[1]
  M2[1] <- 0
  
  # loop over remaining observations
  for (i in 2:n) {
    # update mean
    mean[i] <- mean[i-1] + (x[i] - mean[i-1])/i
    
    # update M2
    M2[i] <- M2[i-1] + (x[i]-mean[i-1])*(x[i]-mean[i])
  }
  
  # get variance from M2
  s2 <- M2[n]/(n-1)
  
  # return as tibble
  return(tibble(mean = mean[n], var = s2))
}

get_mean_var(1:10)

# ways to break our function
y1 <- 1
y2 <- c("a","b","c") # character vector
y3 <- matrix(1:4,nrow=2)
y4 <- data.frame(x1 = 1:2, x2 = 3:4) # data.frame

get_mean_var(y1)
get_mean_var(y2)
get_mean_var(y3)
get_mean_var(y4)