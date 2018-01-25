# Disclosure Risk: Interval Disclosure ------------------------------------

SDID <- function(dat, dat.agreg, sdist = .05) {
   #sdist (safety distance): verificar valor no intervalo [0,1]
   sdist <- unique(sdist)
   dat.aux <- abs(dat - dat.agreg) / apply(dat.agreg, 2, sd)
   SDID <- 
      sapply(sdist, function(x) {
         apply(dat.aux, 1, function(y) all(y <= x))
      }) %>% 
      apply(2, sum) / nrow(dat)
   # names(SDID) <- sdist
   return(SDID * 100)
}