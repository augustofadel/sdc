# degree constrained kruskal MST algorithm --------------------------------

# uniao por rank (p.109)
union_ds <- function(e, S) {
   u <- find_set(e[[1]], S)
   v <- find_set(e[[2]], S)
   if (S[u, 'rank'] == S[v, 'rank']) {
      S[u, 'rank'] <- S[u, 'rank'] + 1
      S[v, 'root'] <- u
   } else {
      if (S[u, 'rank'] > S[v, 'rank']) {
         S[v, 'root'] <- u
      } else {
         S[u, 'root'] <- v
      }
   }
   return(S)
}