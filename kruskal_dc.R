# degree constrained kruskal MST algorithm --------------------------------

kruskal_dc <- function(V, E, w, d) {
   ET <- NULL
   deg <- vector('integer', n)
   S <- matrix(
      data = 0, 
      nrow = length(V), 
      ncol = 2, 
      dimnames = list(V, c('root', 'rank'))
   )
   S[,'root'] <- V
   S[,'rank'] <- 1
   E <- E[order(w),]
   pb <- txtProgressBar(min = 0, max = nrow(E), width = 100, style = 3)
   for (i in 1:nrow(E)) {
      d1 <- deg[E[i, 1]]
      d2 <- deg[E[i, 2]]
      if ((find_set(E[i,1], S) != find_set(E[i,2], S)) & d1 < d & d2 < d) {
         ET <- rbind(ET, E[i,])
         deg[E[i, 1]] <- d1 + 1
         deg[E[i, 2]] <- d2 + 1
         S <- union_ds(E[i,], S)
      }
      setTxtProgressBar(pb, i)
   }
   close(pb)
   return(ET)
}