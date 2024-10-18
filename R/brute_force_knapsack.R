RNGversion(min(as.character(getRversion()),"3.5.3"))

##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

a <- knapsack_objects[1:8,]
a
a$w[1]



brute_force_knapsack <- function(x, W){
  # check input data types
  stopifnot(is.data.frame(x), is.numeric(W), W > 0)
  # check for correct column names and only positive values
  stopifnot(all(which(x$w > 0)), all(which(x$v > 0)))
  
  n <- nrow(x)
  best_weight <- 0
  best_value <- 0
  best_combination <- NULL
  
  # go through every possible combination
  for (i in 0:(2^n - 1)){
    
    weight <- 0
    value <- 0
    
    # create bit string, that indicates whether an item is included or not 
    combination <- intToBits(i)[1:n]
    
    j <- 1
    
    # add weights and values, if they are in the combination
    while (j <= n) {
      if (combination[j] == as.raw(1)) {
        weight <- weight + x$w[j]
        value <- value + x$v[j]
      }
      j <- j + 1
    }
    
    # update the best results, if the new value is better and weight does not exceed limit
    if (value > best_value & weight <= W){
      best_weight <- weight
      best_value <- value
      best_combination <- combination
    }
  }
  
  positions <- which(best_combination == as.raw(1))
  
  return_list <- list(value=best_value, elements=positions)
  return(return_list)
}

system.time(gk <- brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)