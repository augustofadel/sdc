# Information Loss: IL1 ---------------------------------------------------

IL1 <- function(dat, dat.agreg) {
   SSE <-
      (dat - dat.agreg)^2 %>% 
      apply(1, sum) %>% 
      sum()
   centroide <- 
      dat[, lapply(.SD, mean)] %>% 
      as.numeric()
   SST <-
      (sweep(dat, 2, centroide))^2 %>% 
      apply(1, sum) %>% 
      sum()
   return(SSE / SST * 100)
}