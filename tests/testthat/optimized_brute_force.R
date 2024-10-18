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

optimized_brute_force_knapsack <- function(x, W){
  # check input data types
  stopifnot(is.data.frame(x), is.numeric(W), W > 0)
  # check for correct column names and only positive values
  stopifnot(all(which(x$w > 0)), all(which(x$v > 0)))
  
  n <- nrow(x)
  best_weight <- 0
  best_value <- 0
  best_combination <- NULL
  
  # go through every possible combination
  
  v <- 0:(2^n - 1)
  
  calculation <- function(combination){
    #print(combination)
    bit_combination <- intToBits(combination)[1:n]
    #print(bit_combination)
    value <- 0
    weight <- 0
    j <- 1
    
    # add weights and values, if they are in the combination
    while (j <= n) {
      if (bit_combination[j] == as.raw(1)) {
        weight <- weight + x$w[j]
        value <- value + x$v[j]
      }
      j <- j + 1
    }
    
    # update the best results, if the new value is better and weight does not exceed limit
    if (value > best_value & weight <= W){
      # make deep assignments to update the variables in the outer function
      best_weight <<- weight
      best_value <<- value
      best_combination <<- bit_combination
    }
    
    
    return(best_value)
  }
  
  vectorized_calculation <- Vectorize(calculation)
  
  b_v <- vectorized_calculation(v)
  
  positions <- which(best_combination == as.raw(1))
  
  return_list <- list(value=best_value, elements=positions)
  return(return_list)
}


optimized_brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

#n <- 1
#0:(2^n - 1)
#intToBits(1)
#brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)

