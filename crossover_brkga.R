# crossover BRKGA ---------------------------------------------------------
# (Solucoes elite e mutantes sao sempre viaveis. Crossover simples implica na inclusao de muitas solucoes nao viaveis por geracao.)

crossover_brkga <- function(ce, cn, pr) {
   n <- length(ce)
   cf <- vector('integer', n)
   va <- runif(n)
   cf[va <= pr] <- ce[va <= pr]
   cf[va > pr] <- cn[va > pr]
   return(cf)
}