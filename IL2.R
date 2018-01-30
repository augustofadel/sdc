# Information Loss: IL2 [Mateo-Sanz et al. (2004)] ------------------------

IL2 <- function(dat, dat.agreg) {
   # denom <- sqrt(2) * as.numeric(dat[, lapply(.SD, sd)]) #dat normalizado, sd = 1 para toda vairavel
   # IL <-
   #    abs(dat - dat.agreg) %>% 
   #    apply(1, function(x) {x / denom}) %>% 
   #    apply(2, sum) %>% 
   #    sum() / prod(dim(dat))
   
   Sj <- apply(dat, 2, sd)
   IL <- 
      abs(dat - dat.agreg) %>% 
      `/`(sqrt(2) * Sj) %>% 
      apply(2, sum) %>% 
      sum() %>% 
      `/`(prod(dim(dat)))

   return(IL)
}