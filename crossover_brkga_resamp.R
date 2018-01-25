# crossover BRKGA ---------------------------------------------------------
# (Solucoes elite e mutantes sao sempre viaveis. Crossover simples implica na inclusao de muitas solucoes nao viaveis por geracao.)

# !!! REVER: ao descartar solucao filha inviavel, resamp deve selecionar novos pais
crossover_brkga_resamp <- function(ce, cn, pr, k, iter = 100) {
   n <- length(ce)
   cf <- vector('integer', n)
   count <- table(cf)
   i <- 0
   while ((any(count < min(k)) | any(count >= 2*max(k))) & i <= iter) {
      i <- i + 1
      va <- runif(n)
      cf[va <= pr] <- ce[va <= pr]
      cf[va > pr] <- cn[va > pr]
      count <- table(cf)
   }
   return(cf)
}