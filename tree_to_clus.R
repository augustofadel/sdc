# constroi agrupamento a partir de MST ------------------------------------

tree_to_clus <- function(k, g, E) {
   ne <- nrow(E)
   E <- E[sample(1:ne),]
   i <- 1
   while (diameter(g) >= 2 * k & i <= ne) {
      aux <- delete_edges(g, paste0(E[i,], collapse = '|'))
      shortest_component <- components(aux)$csize %>% min()
      if (shortest_component >= k)
         g <- aux
      i <- i + 1
   }
   clus <- components(g)$membership
   return(clus)
}