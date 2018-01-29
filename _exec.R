# pendencias:
# verificar se, em algum grupo, todos os objetos têm o mesmo valor para alguma variavel (sd = 0)



# Inicializacao -----------------------------------------------------------

setwd("~/!filesync/repos/sdc")

# carregar funcoes
func_list <- list.files(pattern = "^[^_].+\\.R$")
sapply(func_list, source, .GlobalEnv)

# carregar/instalar pacotes
pkgs(c(
   'TSP', 
   'igraph', 
   'doParallel', 
   'foreach', 
   'dplyr', 
   'data.table', 
   'pdist', 
   'nsga2R', 
   'coop', 
   'ggplot2',
   'sdcMicro',
   'profvis'
))

# carregar dados
data('Tarragona')
dat <- Tarragona
data('CASCrefmicrodata')
dat <- CASCrefmicrodata
data('EIA')
dat <- EIA[,6:15]


# Execucao BRKGA ----------------------------------------------------------

p <- profvis({
   result <- microagg_brkga(
      arq = dat,                             # dataset
      k_int = c(3, 4, 5, 10),                # vetor niveis de agregacao
      p_k = 50,                              # tamanho da populacao (para cada k)
      tot_ger = 50,                         # numero de geracoes
      d = 'euclidean',                       # metrica distancia
      init = 'TSP',                          # estrategia de inicializacao: TSP, MST, aleat
      multik = T,                            # T = inicializa uma unica populacao com diferentes valores de k
      metricas = c('IL2'),                   # metrica p/ calcular fitness
      pe = .2,                               # proporcao de solucoes elite
      pm = .15,                              # proporcao de solucoes mutantes
      pr = .7,                               # probabilidade de solucao de gene do conjunto elite
      crossover = 'regen',                   # metodo crossover: regen = regeneracao solucoes inviaveis; resamp = resampling
      nuc = 8                                # qtd nucleos paralelizacao
   )
})

htmlwidgets::saveWidget(p, 'microagg_brkga_profile.html')

# Execucao NSGA-II --------------------------------------------------------

result <- microagg_nsga2(
   arq = dat,                               # dataset
   k_int = c(3, 4, 5, 10),                  # vetor niveis de agregacao
   p_k = 50,                                # tamanho da populacao (para cada k)
   max_iter = 200,                          # numero de iteracoes
   d = 'euclidean',                         # metrica distancia
   init = 'TSP',                            # estrategia de inicializacao
   multik = T,                              # T = inicializa uma unica populacao com diferentes valores de k
   metricas = c('DLD', 'SDID', 'IL3'),      # metricas p/ calcular fitness
   nc = 0.1,                                # parametro cruzamento SBX
   nm = 0.1,                                # parametro mutacao polinomial
   nuc = 8                                  # qtd nucleos paralelizacao
)





# Graficos ----------------------------------------------------------------

dat <- data.table(dat)
# sol <- result[[1]]
sol <- do.call('cbind', result)
metricas = c('DLD', 'SDID', 'IL1', 'IL2', 'IL3')
cl <- makeCluster(8)
registerDoParallel(cl)
fitness <- foreach(
   i = 1:ncol(sol),
   .combine = 'cbind',
   .packages = c('data.table', 'dplyr', 'pdist'),
   .export = c('fit', 'agreg', 'DLD', 'SDID', 'IL1', 'IL2', 'IL3')
) %dopar% {
   fit(dat, sol[, i], c('DLD', 'SDID', 'IL1', 'IL2', 'IL3'))
}
stopCluster(cl)

k_resultante <- apply(sol, 2, function(x) {
   table(x) %>% range()
})
result.plot <- 
   rbind(
      fitness,
      k_resultante
   ) %>% 
   t() %>% 
   as.data.frame()
names(result.plot) <- c(metricas, 'k_min', 'k_max')
rownames(result.plot) <- NULL

ggplot(result.plot, aes(DLD, IL1)) + 
   geom_point(aes(colour = factor(k_min)), size = 3) + 
   labs(color = 'k min.', title = 'EIA (BRKGA, pop=50, ger=200, t=804s)') 

# dat.agreg <- agreg(dat, sol[,317])
tapply(result.plot$IL1, result.plot$k_min, min)
