# distancia entre pontos de uma rota (TSP) --------------------------------

city_distance <- function(tour, dissim, n) {
   dissim <- as.matrix(dissim)
   aux <- vector('numeric', n)
   if (any(class(tour) == 'TOUR')) {
      tour <- as.integer(tour)
      aux[n] <- dissim[tour[1], tour[n]]
   } else {
      aux[n] <- -Inf
   }
   for (i in 1:(n - 1)) {
      aux[i] <- dissim[tour[i], tour[i+1]]
   }
   names(aux) <- tour
   return(aux)
}