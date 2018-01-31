# Information Loss: IL2 [Mateo-Sanz et al. (2004)] ------------------------

IL2 <- function(dat, dat.agreg) {
   den <- sqrt(2) * as.numeric(dat[, lapply(.SD, sd)]) #dat normalizado, sd = 1 para toda variavel
   IL <-
      abs(dat - dat.agreg) %>%
      apply(1, function(x) {x / den}) %>%
      sum() / prod(dim(dat))
   return(IL)
}