# binary tournament selection ---------------------------------------------

bin_select <- function(pop_sel, k = k_int[[i]], pool = p_k, size = 2) {
   n <- ncol(pop_sel)
   sel <- 
      matrix(1:n, nrow = n, ncol = pool) %>%
      apply(2, function(x) sample(x, size))
   clus_size <- 
      pop_sel[rownames(pop_sel) == '',] %>% 
      # apply(2, function(x) {aux <- table(x); all(aux >= k & aux < 2*k)})
      apply(2, function(x) {aux <- table(x); sum(aux < k) + sum(aux > 2 * k - 1)})
   vencedores <- apply(sel, 2, function(x) {
      aux <- clus_size[x] * 100 + pop_sel['rank_index', x] / max(pop_sel['rank_index',]) * 10
      if (diff(aux) == 0)
         vencedor <- x[which.max(pop_sel['crowdist', x])]
      else
         vencedor <- x[which.min(aux)]
      return(vencedor)
   })
   return(pop_sel[,vencedores])
}
