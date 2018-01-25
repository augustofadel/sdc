# Information Loss: IL2 ---------------------------------------------------

IL2 <- function(dat, dat.agreg) {
   IL <-
      (abs(dat - dat.agreg) / sqrt(2)) %>% #dat normalizado, apply(dat, 2, sd) = 1
      apply(2, sum) %>% 
      sum() / prod(dim(dat))
   # return(IL)
   IL_max <-
      sweep(dat.agreg, 2, as.numeric(dat[, lapply(.SD, mean)])) %>%
      abs() %>%
      apply(2, sum) %>%
      sum() / prod(dim(dat))
   return(IL / IL_max * 100)
}