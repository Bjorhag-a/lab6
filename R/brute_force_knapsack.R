#' @title Brute Force Knapsack Solver
#' 
#' @description
#' This function solves the knapsack problem by a Brute force search and look
#' through every combination of items to find the max value without overstep the
#' weight limit.
#' 
#'
#' @param x A data frame with two variables, the value of the item and the weight of the item. 
#' @param W A numeric number that sets the total weight capacity of the knapsack and must be greater than 0
#' @param fast logical, Set to TRUE and a fast C++ implementation calculates the
#'   algorithm. Default is FALSE
#'
#' @returns A list of the selected items total value and a vector of the selected items observation number
#' @export
#'
#' @importFrom Rcpp cppFunction
#'
#' 
#' @examples 
#' # Example with a dataset
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' ##old sampler used for backward compatibility
#' # suppressWarnings() can be used so that the above warning is not displayed
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <-
#' data.frame(
#'  w=sample(1:4000, size = n, replace = TRUE),
#' v=runif(n = n, 0, 10000)
#' )
#' 
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, fast = TRUE)
#' 
#' 
brute_force_knapsack <- function(x, W, fast=FALSE){
  # check input data types
  stopifnot(is.data.frame(x), is.numeric(W), W > 0)
  # check for correct column names and only positive values
  stopifnot(all(which(x$w > 0)), all(which(x$v > 0)))
  
  n <- nrow(x)
  w <- x$w
  v <- x$v
  
  if (fast==FALSE) {
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
          weight <- weight + w[j]
          value <- value + v[j]
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
  }
  else {
    cppFunction(
    'List bruteForceRcpp(int n, NumericVector w, NumericVector v, int W) {
  double best_weight = 0;
  double best_value = 0;
  IntegerVector best_combination(n);
  
  // Go through every possible combination (from 0 to 2^n - 1)
  for (int i = 0; i < (1 << n); ++i) {
    double weight = 0;
    double value = 0;
    IntegerVector combination(n);
    
    // Go through each item to determine if it is included in the current combination
    for (int j = 0; j < n; ++j) {
      // Check if j-th bit in i is set (1 means the item is included in the combination)
      if (i & (1 << j)) {
        weight += w[j];
        value += v[j];
        combination[j] = 1;
      }
    }
    
    // Update the best result if the current combination is valid and better
    if (weight <= W && value > best_value) {
      best_weight = weight;
      best_value = value;
      best_combination = combination;
    }
  }
  
  // Return the best weight, best value, and best combination as a list
  return List::create(
    Named("best_weight") = best_weight,
    Named("best_value") = best_value,
    Named("best_combination") = best_combination
  );
}'
    )
    
    result_list <- bruteForceRcpp(n, w, v, W)
    best_weight <- result_list$best_weight
    best_value <- result_list$best_value
    best_combination <- result_list$best_combination
    #print(class(best_combination))

    positions <- which(best_combination == 1)
  }
  
  
  return_list <- list(value=best_value, elements=positions)
  return(return_list)
}



tmp <- function(x, W, fast = FALSE){
  # Check input data types and constraints
  stopifnot(is.data.frame(x), is.numeric(W), W > 0)
  stopifnot(all(x$w > 0), all(x$v > 0))  # Ensure only positive values for weights and values
  
  n <- nrow(x)
  w <- x$w
  v <- x$v
  
  # Generate all possible combinations as a matrix
  all_combinations <- as.matrix(expand.grid(rep(list(c(0, 1)), n)))

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







# profvis({
#   x <- knapsack_objects[1:12,]
#   W <- 3500
# 
#   n <- nrow(x)
#   best_weight <- 0
#   best_value <- 0
#   best_combination <- NULL
# 
#   # go through every possible combination
#   for (i in 0:(2^n - 1)){
# 
#     weight <- 0
#     value <- 0
# 
#     # create bit string, that indicates whether an item is included or not
#     combination <- intToBits(i)[1:n]
# 
#     j <- 1
# 
#     # add weights and values, if they are in the combination
#     while (j <= n) {
#       if (combination[j] == as.raw(1)) {
#         weight <- weight + x$w[j]
#         value <- value + x$v[j]
#       }
#       j <- j + 1
#     }
# 
#     # update the best results, if the new value is better and weight does not exceed limit
#     if (value > best_value & weight <= W){
#       best_weight <- weight
#       best_value <- value
#       best_combination <- combination
#     }
#   }
# 
#   positions <- which(best_combination == as.raw(1))
# 
#   return_list <- list(value=best_value, elements=positions)
# })
