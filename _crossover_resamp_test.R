# verifica se ha solucao inviavel
k <- c(3, 4, 5, 10)
pop <- 
   result[[1]] %>% 
   apply(2,sort, method = "radix")
valid <- apply(pop, 2, function(x) {
   group_count <- table(x)
   if ((any(group_count >= min(k)) & any(group_count < 2*max(k))))
      valid <- 1
   else
      valid <- 0
   return(valid)
})
# proporcao de solucoes validas na geracao:
sum(valid) / ncol(pop)


# crossover simples
crossover <- function(ce, cn, pr, k) {
   n <- length(ce)
   cf <- vector('integer', n)
   va <- runif(n)
   cf[va <= pr] <- ce[va <= pr]
   cf[va > pr] <- cn[va > pr]
   group_count <- table(cf)
   if ((any(group_count < min(k)) | any(group_count >= 2*max(k))))
      valid <- 0
   else
      valid <- 1
   return(valid)
}


# 100.000 execucoes crossover simples
size_pop <- ncol(pop)
size_elit <- .2 * size_pop
elit <- pop[-(n + 1), 1:size_elit]
nelit <- pop[-(n + 1), (size_elit + 1):size_pop]

size_cros <- 100000

parents <- cbind(
   sample(1:ncol(elit), size_cros, replace = T),
   sample(1:ncol(nelit), size_cros, replace = T)
)
valid <- apply(
   parents, 1, 
   function(x) crossover(
      ce = elit[, x[1]], 
      cn = nelit[, x[2]], 
      pr = pr, 
      k = k
   )
)

table(valid)










library(foreach)

size_cros <- 100000

parents <- cbind(
   sample(1:ncol(elit), size_cros, replace = T),
   sample(1:ncol(nelit), size_cros, replace = T)
)

cl <- makeCluster(8)
registerDoParallel(cl)

prop_valid <-
   foreach(
      i = 1:nrow(parents),
      .init = 0,
      .combine = '+',
      .export = c('crossover_brkga')
   ) %dopar% {
      crossover_brkga(
         ce = elit[, parents[i,1]], 
         cn = nelit[, parents[i,2]], 
         pr = 0.7, 
         k = c(3, 4, 5, 10)
      )
   }

stopCluster(cl)

valid / size_pop
