# simulated binary crossover ----------------------------------------------
# (SBX - Deb, Agrawal; 1995)

cross_sbx <- function(p1, p2, nc) {
   u <- runif(1)
   if (u <= 0.5) {
      p <- (2 * u)^(1 / (nc - 1))
   } else {
      p <- (1 / (2 * (1 - u)))^(1 / (nc - 1))
   }
   q1 <- round(0.5 * ((1 + p) * p1 + (1 - p) * p2))
   q2 <- round(0.5 * ((1 - p) * p1 + (1 + p) * p2))
   return(cbind(q1, q2))
}