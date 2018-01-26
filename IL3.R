# Information Loss: IL3 ---------------------------------------------------
# Domingo-Ferrer e Torra (2001) p.8

IL3 <- function(dat, dat.agreg) {
   n <- nrow(dat)
   p <- ncol(dat)
   IL <- vector('numeric', 5L)
   
   IL[1] <-
      (abs(dat - dat.agreg) / abs(dat)) %>% 
      apply(2, sum) %>% 
      sum() %>% 
      `/`(n * p)
   
   dat.mean <- dat[, lapply(.SD, mean)]
   dat.agreg.mean <- dat.agreg[, lapply(.SD, mean)]
   
   IL[2] <- # verificar a necesssidade desse termo
      (abs(dat.mean - dat.agreg.mean) / abs(dat.mean)) %>% 
      sum() %>% 
      `/`(p)
   
   dat.cov <- cov(dat)
   dat.agreg.cov <- cov(dat.agreg)
   triang.sup <- upper.tri(dat.cov, diag = T)
   
   IL[3] <- 
      (abs(dat.cov[triang.sup] - dat.agreg.cov[triang.sup]) / abs(dat.cov[triang.sup])) %>% 
      sum() %>% 
      `/`((p * (p + 1))/2)
   
   IL[4] <- 
      (abs(diag(dat.cov) - diag(dat.agreg.cov)) / diag(dat.cov)) %>% #dat normalizado
      sum() %>% 
      `/`(p)
   
   dat.cor <- cor(dat)
   dat.agreg.cor <- cor(dat.agreg)
   
   IL[5] <- 
      (abs(dat.cor[triang.sup] - dat.agreg.cor[triang.sup])) %>% 
      sum() %>% 
      `/`((p * (p + 1))/2)
   
   return(sum(IL) / 5)
}