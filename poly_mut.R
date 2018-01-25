# Polynomial Mutation -----------------------------------------------------

poly_mut <- function(p1, nm, k = k_int[i]) {
   u <- runif(1)
   if (u < 0.5) {
      p <- (2 * u)^(1 / (nm + 1)) - 1
   } else {
      p <- 1 - (2 * (1 - u))^(1 / (nm + 1))
   }
   q1 <- round(p1 + p * k)
   return(q1)
}