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



Greedy_heuristic <-function(x, W){
  
  val_per_w <- x$v / x$w
  x$val_per_w <- val_per_w
  x <- x[order(x$val_per_w, decreasing = T), ]
  
  total_v <- c()
  n <- nrow(x)
  
  
  
  for (i in 1:n) {
    if (total_v < W) {
      
    }
    
  }
  
  return(list(value = balbla, elements = jajaj))
  
}





test <- knapsack_objects[1:800,]
val_per_w <- test$v / test$w
test$val_per_w <- val_per_w
test <- test[order(val_per_w, decreasing = T), ]


greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)