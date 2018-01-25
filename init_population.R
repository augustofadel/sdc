# inicializa populacao ----------------------------------------------------

# (segmenta os objetos em grupos, segundo ordem predefinida ou arvore geradora)
init_population <- function(k, n, p, input) {
   if (class(input) == 'data.frame' && dim(input) == c(n - 1, 2)) {
      E <- input
      g <- graph_from_edgelist(as.matrix(E), directed = F)
      pop <- sapply(
         rep(k, p), 
         function(x) tree_to_clus(x, g, E)
      )
   } else {
      if (class(input) == 'integer' & length(input) == n) {
         ord <- input
         pop <- sapply(
            rep(k, p), 
            function(x) init_individual(x, n)
         ) %>% apply(2, function (x) x[ord])
      } else {
         stop('Argumentos incorretos.')
      }
   }
   return(pop)
}