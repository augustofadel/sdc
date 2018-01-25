# crossover BRKGA ---------------------------------------------------------
# (Solucoes elite e mutantes sao sempre viaveis. Crossover simples implica na inclusao de muitas solucoes nao viaveis por geracao.)

crossover_brkga_regen <- function(ce, cn, pr, k, iter = 100) {
   n <- length(ce)
   cf <- vector('integer', n)
   va <- runif(n)
   cf[va <= pr] <- ce[va <= pr]
   cf[va > pr] <- cn[va > pr]
   count <- tabulate(cf)
   i <- 0
   while ((any(count < min(k)) | any(count >= 2 * max(k))) & i <= iter) {
      i <- i + 1
      recep <- which.min(count)
      donat <- which.max(count)
      recep_qty <- max(0, min(k) - count[recep])             # numero de objetos que o grupo precisar receber para se tornar viavel
      donat_qty <- max(0, count[donat] - (2 * max(k) - 1))   # numero de objetos que o grupo precisar doar para se tornar viavel
      qty <- min(
         recep_qty + max(k) - 1,                             # maximo de objetos que o grupo pode receber sem se tornar inviavel
         donat_qty                                           # numero de objetos que o grupo precisar doar para se tornar viavel
      )
      cf[cf == donat][1:qty] <- recep
      count <- tabulate(cf)
   }
   return(cf)
}