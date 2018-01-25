# Information Loss: IL2 relativa ------------------------------------------

IL2_r <- function(dat, dat.agreg) {
   denom <- sqrt(2) * as.numeric(dat[, lapply(.SD, sd)]) #dat normalizado, sd = 1 para toda vairavel
   IL <-
      abs(dat - dat.agreg) %>% 
      apply(1, function(x) {x / denom}) %>% 
      apply(2, sum) %>% 
      sum() / ncol(dat)
   # return(IL)
   IL_max <-
      sweep(dat.agreg, 2, as.numeric(dat[, lapply(.SD, mean)])) %>%  # dat.agreg[i,] - mean(dat[,j])
      abs() %>%
      apply(1, function(x) {x / denom}) %>% 
      apply(2, sum) %>%
      sum() / ncol(dat)
   return(IL / IL_max * 100)
}