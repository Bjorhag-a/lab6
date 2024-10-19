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


#TODO suppress warnings for RNG



#' @title Dynamic Programming Knapsack Solver
#' 
#' @description
#' This function solves the knapsack problem using dynamic programming maximize
#' the value of items without overstep the weight limit
#' 
#'
#' @param x A data frame with two variables, the value of the item and the weight of the item. 
#' @param W A numeric number that sets the total weight capacity of the knapsack and must be greater than 0
#' @param fast logical, Set to TRUE and a fast C++ implementation calculates the
#'   algorithm. Default is FALSE
#'
#' @returns A list of the selected items total value and a vector of the selected items observation number
#' @export
#' @importFrom Rcpp cppFunction
#'
#' @examples 
#' Example with a dataset
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' ##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <-
#' data.frame(
#'  w=sample(1:4000, size = n, replace = TRUE),
#' v=runif(n = n, 0, 10000)
#' )
#' 
#' knapsack_dynamic(knapsack_objects[1:8,], W = 3500, fast = FALSE)
#' 
knapsack_dynamic <- function(x, W, fast=FALSE){
  # check input data types
  stopifnot(is.data.frame(x), is.numeric(W), W > 0)
  # check for correct column names and only positive values
  stopifnot(all(which(x$w > 0)), all(which(x$v > 0)))
  
  
  n <- nrow(x)
  w <- x$w
  v <- x$v
  m <- NULL
  
  if (fast==FALSE) {
    m <- matrix(0, nrow=n+1, ncol=W+1)
    
    for (i in 2:n){
      for (j in 2:W){
        if (w[i] > j) {
          m[i,j] <- m[i-1,j]
        }
        else{
          m[i,j] <- max(m[i-1,j], m[i-1,j-w[i]]+v[i])
        }
      }
    }
  }
  
  else {
    cppFunction(
      'NumericMatrix knapsackCpp(int n, int W, NumericVector w, NumericVector v) {
    NumericMatrix m(n + 1, W + 1);
    
    // Loop over the items
    for (int i = 1; i < n; i++) {
      for (int j = 1; j < W; j++) {
        if (w[i] > j) {
          m(i, j) = m(i - 1, j);
        } else {
          m(i, j) = std::max(m(i - 1, j), m(i - 1, j - w[i]) + v[i]);
        }
      }
    }
    
    return m;
  }'
    )
    m <- knapsackCpp(n, W, w, v)
  }
  
  find_combination <- function(i, j){
    if (i == 1) {
      return (c())
    }
    if (m[i,j] > m[i-1,j]){
      return (c(i,find_combination(i-1, j-w[i])))
    }
    else{
      return (find_combination(i-1, j))
    }
  }
  
  combination <- sort(find_combination(n, W))
  
  return(list(value=m[n,W], elements=combination))
}

system.time(gk <- knapsack_dynamic(x = knapsack_objects[1:500,], W = 2000, fast = TRUE))
system.time(gk <- knapsack_dynamic(x = knapsack_objects[1:500,], W = 2000, fast = FALSE))
knapsack_dynamic(knapsack_objects[1:8,], 3500)


library("profvis")
profvis({
  x <- knapsack_objects[1:500,]
  W <- 2000
  
  n <- nrow(x)
  w <- x$w
  v <- x$v
  
  m <- matrix(0, nrow=n+1, ncol=W+1)
  
  for (i in 2:n){
    for (j in 2:W){
      if (w[i] > j) {
        m[i,j] <- m[i-1,j]
      }
      else{
        m[i,j] <- max(m[i-1,j], m[i-1,j-w[i]]+v[i])
      }
    }
  }
  
  find_combination <- function(i, j){
    if (i == 1) {
      return (c())
    }
    if (m[i,j] > m[i-1,j]){
      return (c(i,find_combination(i-1, j-w[i])))
    }
    else{
      return (find_combination(i-1, j))
    }
  }
  
  combination <- sort(find_combination(n, W))
})