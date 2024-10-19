#' @title Optimized Brute Force Knapsack Solver
#' 
#' @description
#' This function solves the knapsack problem by a Brute force search and look
#' through every combination of items to find the max value without overstep the
#' weight limit. It creates a matrix with all possible combinations and performs
#' vectorized functions on it. So it still calculates all values, but uses the 
#' advantages of the faster operations.
#' 
#'
#' @param x A data frame with two variables, the value of the item and the weight of the item. 
#' @param W A numeric number that sets the total weight capacity of the knapsack and must be greater than 0
#'
#' @returns A list of the selected items total value and a vector of the selected items observation number
#' @export
#'
#'
#' 
#' @examples 
#' # Example with a dataset
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' ##old sampler used for backward compatibility
#' # suppressWarnings() can be used so that the above warning is not displayed
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <-data.frame(
#'  w=sample(1:4000, size = n, replace = TRUE),
#' v=runif(n = n, 0, 10000)
#' )
#' 
#' optimized_brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' 
#' 


optimized_brute_force_knapsack <- function(x, W){
  # check input data types
  stopifnot(is.data.frame(x), is.numeric(W), W > 0)
  # check for correct column names and only positive values
  stopifnot(all(which(x$w > 0)), all(which(x$v > 0)))
  
  n <- nrow(x)
  w <- x$w
  v <- x$v
  
  # Generate all possible combinations as a matrix
  all_combinations <- as.matrix(expand.grid(rep(list(c(0, 1)), n)))
  
  # use vectorized functions for calculations
  total_weights <- all_combinations %*% w
  total_values <- all_combinations %*% v
  
  # Find the best combination
  within_capacity <- total_weights <= W
  
  best_value <- max(total_values[within_capacity])
  best_index <- which.max(total_values[within_capacity])
  
  best_combination <- all_combinations[within_capacity, ][best_index, ]
  best_weight <- total_weights[within_capacity][best_index]
  
  positions <- as.numeric(which(best_combination == 1))
  
  
  return_list <- list(value = best_value, elements = positions)
  return(return_list)
}

