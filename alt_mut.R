# mutacao alternativa -----------------------------------------------------

alt_mut <- function(p1, p_mut = 0.05) {
   n <- length(p1)
   index <- sample(1:n, round(n * p_mut))
   p1[index] <- sample(p1[index])
   return(p1)
}