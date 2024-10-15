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



greedy_knapsack <-function(x, W){
  
  val_per_w <- x$v / x$w
  x$val_per_w <- val_per_w
  x <- x[order(x$val_per_w, decreasing = T), ]
  
  total_v <- 0
  total_w <- 0
  n <- nrow(x)
  elements <- c()
  
  
  
  for (i in 1:n) {
    
    if (sum(total_w) + x[i, 1] < W) {
      total_v <- total_v + x[i, 2]
      print(total_v)
      total_w <- total_w + x[i, 1]
      
    } else {
      names(x)[i]
      break
    }
    
  }
  
  print(total_w)
  return(list(value = total_v))
  
}

greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)



test <- knapsack_objects[1:800,]
val_per_w <- test$v / test$w
test$val_per_w <- val_per_w
test <- test[order(val_per_w, decreasing = T), ]
test[1, 3]
total_v <- 0
total_w <- 0
W <- 3500
for (i in 1:800) {
  
  total_v <- total_v + test[i, 3]
  total_w <- total_w + test[i, 1]
  
  if (sum(total_w) < W) {
    
    print(sum(total_v))
  }
  
}

greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)