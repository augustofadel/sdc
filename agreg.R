# gera dataset agregado, segundo vetor de alocacao (cromossomo) -----------

agreg <- function(dat, clus) {
   clus <- data.table(clus)
   dat.agreg <- dat[, lapply(.SD, mean), by = clus]
   dat.agreg <- 
      merge(clus, dat.agreg, by = 'clus', all = T, sort = F) %>% 
      dplyr::select(-clus)
   return(dat.agreg)
}