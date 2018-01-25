# degree constrained kruskal MST algorithm --------------------------------

# sem compressao de caminhos
find_set <- function(u, S) {
   if (S[u, 'root'] != u) {
      S[u, 'root'] <- find_set(S[u, 'root'], S)
   }
   return(S[u, 'root'])
}