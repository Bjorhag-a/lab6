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

library("Rcpp")

#' @title Greedy heuristic Knapsack problem solver
#' @description
#' This function solves the knapsack problem using the greedy algorithm. The algorithm selects 
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
#' No C++ implementation example
#' greedy_knapsack(x = knapsack_objects[1:8,], W = 3500, fast = FALSE)

##old sampler used for backward compatibility
## suppressWarnings() can be used so that the above warning is not displayed
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
#' 
#' 
greedy_knapsack <-function(x, W, fast=FALSE){
  
  # check input data types
  stopifnot(is.data.frame(x), is.numeric(W), W > 0)
  # check for correct column names and only positive values
  stopifnot(all(which(x$w > 0)), all(which(x$v > 0)))
  
  v <- x$v
  w <- x$w
  
  if (fast==FALSE) {
    #Transform data and sort them so the for loop works as it should. 
    val_per_w <- v / w
    x$val_per_w <- val_per_w
    x <- x[order(x$val_per_w, decreasing = T), ]
    
    total_v <- 0
    total_w <- 0
    n <- nrow(x)
    elements <- c()
    
    
    #For loop adds the val per w till the weight reatch the limit set.
    for (i in 1:n) {
      
      if (sum(total_w) + x[i, 1] < W) {
        total_v <- total_v + x[i, 2]
        #print(total_v)
        total_w <- total_w + x[i, 1]
        
      } else {
        break
      }
      elements <- c(elements, as.numeric(row.names(x)[i]))
    }
  }
  
  else {
    cppFunction('
      List knapsackCpp(NumericVector v, NumericVector w, int W){
        int n = v.size();
    
        std::vector<std::pair<double, int>> val_per_w(n);
        
        for (int i = 0; i < n; i++) {
          val_per_w[i] = std::make_pair(v[i] / w[i], i);
        }
        
        std::sort(val_per_w.begin(), val_per_w.end(), std::greater<std::pair<double, int>>());
        
        double total_v = 0.0;
        double total_w = 0.0;
        std::vector<int> elements;
        
        for (int i = 0; i < n; i++) {
          int idx = val_per_w[i].second; 
          
          if (total_w + w[idx] < W) {
            total_v += v[idx];
            total_w += w[idx];
            elements.push_back(idx + 1);  
          } else {
            break;
          }
        }
        return List::create(
          Named("total_value") = total_v,
          Named("total_weight") = total_w,
          Named("elements") = elements
        );
      }           
    ')
    
    result <- knapsackCpp(v, w, W)
    
    total_v <- result$total_value
    elements <- result$elements
  }
  
  
  #print(elements)
  
  return(list(value = total_v, elements = elements))
  
}

system.time(gk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500))
system.time(gk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500, fast = TRUE))

greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)


greedy_knapsack(x = knapsack_objects[1:800,], W = 3500, fast = TRUE)






#install.packages("profvis")
library(profvis)


p <- profvis(greedy_knapsack(x = knapsack_objects[1:1200,], W = 3500))

p


profvis({
  x <- knapsack_objects[1:1200,]
  W <- 3500
  
  val_per_w <- x$v / x$w
  x$val_per_w <- val_per_w
  x <- x[order(x$val_per_w, decreasing = T), ]
  
  total_v <- 0
  total_w <- 0
  n <- nrow(x)
  elements <- c()
  
  
  #For loop adds the val per w till the weight retch the limit set.
  for (i in 1:n) {
    
    if (sum(total_w) + x[i, 1] < W) {
      total_v <- total_v + x[i, 2]
      #print(total_v)
      total_w <- total_w + x[i, 1]
      
    } else {
      break
    }
    elements <- c(elements, as.numeric(row.names(x)[i]))
  }
})
