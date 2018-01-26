# crossover BRKGA ---------------------------------------------------------
# (Solucoes elite e mutantes sao sempre viaveis. Crossover simples implica na inclusao de muitas solucoes nao viaveis por geracao.)

# !!! REVER: ao descartar solucao filha inviavel, resamp deve selecionar novos pais
crossover_brkga_resamp <- function(ce, cn, pr, k) {
   n <- length(ce)
   cf <- vector('integer', n)
   va <- runif(n)
   cf[va <= pr] <- ce[va <= pr]
   cf[va > pr] <- cn[va > pr]
   group_count <- table(cf)
   if ((any(group_count < min(k)) | any(group_count >= 2*max(k))))
      cf[1] <- 0
   return(cf)
}