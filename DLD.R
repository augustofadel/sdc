# Disclosure Risk: Linkage Disclosure -------------------------------------

DLD <- function(dat, dat.agreg) {
   # distancia euclidiana entre objetos originais e agregados
   d <- 
      pdist(dat.agreg, dat) %>% 
      as.matrix()
   DLD <- sum(apply(d, 1, which.min) == 1:nrow(dat)) / nrow(dat)
   return(DLD * 100)
}