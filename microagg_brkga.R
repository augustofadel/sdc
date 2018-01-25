# microagregacao multivarida mono-objetivo
# inicializacao: TSP, MST ou aleatoria
# otimizacao atraves do BRKGA

# pendencias:
# VERIFICAR CALCULO IL3
# VERIFICAR CROSSOVER COM RESAMPLING (SELEIONAR NOVOS PAIS)

microagg_brkga <- function


# Parametros --------------------------------------------------------------
(
   arq, 
   k_int = c(3, 4, 5, 10), 
   p_k = 50, 
   tot_ger = 500, 
   d = "euclidean", 
   init = 'TSP',
   multik = T,
   metricas = c('IL1'), #c('DLD', 'SDID', 'IL1', 'IL2', 'IL3')
   pe = .2, 
   pm = .15, 
   pr = .7, 
   nuc = 8
) {  
   
   
   # Data input --------------------------------------------------------------
   
   tipo <- sapply(arq, class) %in% c('numeric', 'integer')
   if (!all(tipo)) warning('Atributos nao numericos ignorados.')
   dat <- 
      arq[, tipo] %>% 
      scale() %>% 
      data.table()
   n <- nrow(dat)
   
   
   # Inicializacao -----------------------------------------------------------
   
   cat('\nGerando populacoes iniciais:\n')
   
   # sequencia de objetos atraves de rota TSP (Mortazavi e Jalili (2014))
   if (init == 'TSP') {
      cat('\nCalculando matriz de distancias... ')
      dissim <- 
         dat %>% 
         dist(method = d)
      cat('concluido.\n')
      cat('\nCalculando ciclo TSP... ')
      tour <- 
         dissim %>% 
         TSP() %>% 
         solve_TSP(method = "farthest_insertion", two_opt = F)
      cat('concluido.\n')
      distance_vec <- city_distance(
         tour = tour, 
         dissim = dissim, 
         n = n
      )
      caminho <- tour_to_path(
         tour = tour, 
         distance_vec = distance_vec, 
         n = n
      )
   }
   
   # sequencia de objetos atraves de AGM (sugestao Prof. Satoru)
   if (init == 'MST') {
      cat('\nCalculando matriz de distancias... ')
      dissim <- 
         dat %>% 
         dist(method = d) %>% 
         as.matrix()
      cat('concluido.\n')
      cat('\nConstruindo grafo... ')
      V <- 1:n
      E <- expand.grid(V, V)
      E <- E[E[,1] != E[,2],]
      w <- 
         E %>% 
         apply(1, function(x) dissim[x[1], x[2]]) %>% 
         unname()
      cat('concluido.\n')
      cat('\nObtendo arvore geradora minima...\n')
      ag <- kruskal_dc(V, E, w, 2 * min(k))
      cat('\nconcluido.\n')
   }
   
   # sequencia aleatoria
   if (init == 'rand') {
      caminho <- sample(1:n, replace = F)
   }
   
   # contrucao populacao inicial
   if (exists('caminho')) {
      input <- order(caminho, method = 'radix')
   } else {
      if (exists('ag')) input <- ag
      else stop('Nenhum elemento de inicializacao encontrado.')
   }
   k_int <- as.list(k_int)
   pop <- lapply(
      k_int,
      function(x) init_population(
         k = x, 
         n = n, 
         p = p_k, 
         input = input
      )
   )
   names(pop) <- k_int
   if (multik) {
      pop <- list(do.call('cbind', pop))
      sol <- vector('list', 1)
      names(sol) <- names(pop) <- 'multik'
   } else {
      sol <- vector('list', length(k_int))
      names(sol) <- paste0('k=', k_int)
   }
   cat('\nCalculando fitness... ')
   cl <- makeCluster(nuc)
   registerDoParallel(cl)
   for (i in 1:length(pop)) {
      fitness <-
         foreach(
            j = 1:ncol(pop[[i]]),
            .combine = 'cbind',
            .packages = c('data.table', 'dplyr', 'pdist'),
            .export = c('fit', 'agreg', 'DLD', 'SDID', 'IL1', 'IL2', 'IL3')
         ) %dopar% {
            fit(dat, pop[[i]][, j], metricas)
         }
      pop[[i]] <- rbind(
         pop[[i]],
         fitness
      )[, order(fitness)]
   }
   # stopCluster(cl)
   cat('concluido.\n')
   cat('\nGeracao concluida.\n')
   
   
   # Otimizacao --------------------------------------------------------------
   
   # cl <- makeCluster(nuc)
   # registerDoParallel(cl)
   for (i in 1:length(sol)) {
      cat('\nnivel de agregacao', names(sol)[i], '\n\n')
      pop_atual <- pop[[i]]
      size_pop <- ncol(pop_atual)
      size_elit <- round(pe * size_pop)
      size_mutant <- round(pm * size_pop)
      k <- ifelse(multik, unlist(k_int), k_int[[i]])
      geracao <- 0
      while (geracao < tot_ger) {
         geracao <- geracao + 1
         cat('processando geracao', geracao, '\n')
         cat('   _obtendo conjuntos de solucoes elite e nao elite\n')
         elit <- pop_atual[-(n + 1), 1:size_elit]
         nelit <- pop_atual[-(n + 1), (size_elit + 1):size_pop]
         cat('   _obtendo conjunto de solucoes mutantes\n')
         if (multik) {
            mutant <- lapply(
               k_int,
               function(x) init_population(
                  k = x, 
                  n = n, 
                  p = round(size_mutant/length(k_int)), 
                  input = input
               )
            )
            mutant <- do.call('cbind', mutant)
         } else {
            mutant <- init_population(k_int[[i]], n, size_mutant, input)
         }
         cat('   _executando crossover\n')
         size_cros <- size_pop - ncol(elit) - ncol(mutant)
         parents <- cbind(
            sample(1:ncol(elit), size_cros, replace = T),
            sample(1:ncol(nelit), size_cros, replace = T)
         )
         children <- apply(
            parents, 1, 
            function(x) crossover_brkga_regen(
               ce = elit[, x[1]], 
               cn = nelit[, x[2]], 
               pr = pr, 
               k = k,
               iter = 100
            )
         )
         pop_nova <- cbind(
            children, 
            mutant
         )
         cat('   _calculando fitness\n')
         fitness <-
            foreach(
               j = 1:ncol(pop_nova),
               .combine = 'cbind',
               .packages = c('data.table', 'dplyr', 'pdist'),
               .export = c('fit', 'agreg', 'DLD', 'SDID', 'IL1', 'IL2', 'IL3')
            ) %dopar% {
               fit(dat, pop_nova[, j], metricas)
            }
         pop_nova <- rbind(
            pop_nova,
            fitness
         )
         cat('   _compondo populacao da proxima geracao\n')
         pop_atual <- cbind(
            pop_atual[, 1:size_elit],
            pop_nova
         )
         pop_atual <- pop_atual[, order(pop_atual[n + 1,])]
      } #end_while
      sol[[i]] <- pop_atual[-(n + 1),]
      dimnames(sol[[i]]) <- NULL
   } #end_for
   cat('\nConcluido.\n\n')
   stopCluster(cl)
   return(sol)
}