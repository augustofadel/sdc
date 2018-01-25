# Information Loss: IL3 ---------------------------------------------------

IL3 <- function(dat, dat.agreg) {
   n <- nrow(dat)
   p <- ncol(dat)
   IL <- vector('numeric', 5L)
   
   IL[1] <-
      (abs(dat - dat.agreg) / abs(dat)) %>% 
      apply(2, sum) %>% 
      sum() / (n * p)
   
   dat.mean <- dat[, lapply(.SD, mean)]
   
   IL[2] <- sum(
      abs(dat.mean - dat.agreg[, lapply(.SD, mean)]) / abs(dat.mean)
   ) / p
   
   dat.cov <- cov(dat)
   dat.agreg.cov <- cov(dat.agreg)
   triang.sup <- upper.tri(dat.cov, diag = T)
   
   IL[3] <- 
      sum(
         abs(dat.cov[triang.sup] - dat.agreg.cov[triang.sup]) / abs(dat.cov[triang.sup])
      ) / ((p * (p + 1))/2)
   
   IL[4] <- 
      sum(
         abs(diag(dat.cov) - diag(dat.agreg.cov)) #dat normalizado
      ) / p
   
   dat.cor <- cor(dat)
   dat.agreg.cor <- cor(dat.agreg)
   
   IL[5] <- 
      sum(
         abs(dat.cor[triang.sup] - dat.agreg.cor[triang.sup])
      ) / ((p * (p + 1))/2)
   
   return(sum(IL) / 5)
}