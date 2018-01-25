# inicializa individuo a partir de ordenacao ------------------------------

init_individual <- function(k, n) {
   # define aleatoriamente tamanho dos grupos, respeitando k <= n_i < 2*k
   n_i <- sample(k:(2*k-1), ceiling(n/k), replace = T)
   # aloca objetos sucessivamente conforme grupos definidos
   aloc <- rep.int(1:length(n_i), n_i)[1:n]
   # verifica se ultimo grupo tem tamanho maior ou igual a k, senao concatena ele com o grupo imediatamente anterior
   if(length(unique(aloc[(n-k+1):n])) > 1) {
      aloc[(n-k+1):n] <- aloc[n-k+1]
   }
   return(aloc)
}