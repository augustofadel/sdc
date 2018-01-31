# Information Loss: IL2 relativa ------------------------------------------

IL2_r <- function(dat, dat.agreg) {
   den <- sqrt(2) * as.numeric(dat[, lapply(.SD, sd)]) #dat normalizado, sd = 1 para toda vairavel
   IL <-
      abs(dat - dat.agreg) %>% 
      apply(1, function(x) {x / den}) %>% 
      sum() / prod(dim(dat))
   centroide <- 
      dat[, lapply(.SD, mean)] %>% 
      as.numeric()
   IL_max <-
      sweep(dat.agreg, 2, centroide) %>%
      abs() %>%
      apply(1, function(x) {x / den}) %>% 
      sum() / prod(dim(dat))
   return(IL / IL_max * 100)
}