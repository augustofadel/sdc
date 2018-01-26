library(profvis)

# MEAN --------------------------------------------------------------------


data <- matrix(rnorm(4e5 * 150, mean = 5), ncol = 150)
df <- as.data.frame(data)
dt <- as.data.table(data)

profvis({
   means1 <- apply(data, 2, mean)
   means2 <- apply(df, 2, mean)
   means3 <- apply(dt, 2, mean)
   means4 <- colMeans(data)
   means5 <- colMeans(df)
   means6 <- colMeans(dt)
   means7 <- lapply(df, mean)
   means8 <- lapply(dt, mean)
   means9 <- vapply(df, mean, numeric(1))
   means10 <- vapply(dt, mean, numeric(1))
   means11 <- dt[, lapply(.SD, mean)]
})
