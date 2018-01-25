# fitness -----------------------------------------------------------------

fit <- function(dat, clus, metricas = c('DLD', 'SDID', 'IL1', 'IL2', 'IL3')) {
   dat.agreg <- agreg(dat, clus)
   fit.vec <- sapply(metricas, function(x) do.call(x, list(dat, dat.agreg)))
   return(fit.vec)
}