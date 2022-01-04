### Title:    Make Example Data
### Author:   Kyle M. Lang
### Created:  2016-JAN-28
### Modified: 2016-JAN-28

library(mvtnorm)
dataDir <- "../data/"

rSquared <- 0.5
X <- rmvnorm(100, c(0, 0), diag(2))
beta <- matrix(c(0.5, 0.25))

eta <- X %*% beta
sigma <- (var(eta) / rSquared) - var(eta)

y <- eta + rnorm(100, 0, sqrt(sigma))

simData1 <- data.frame(y, X)
colnames(simData1) <- c("y", "x", "z")

summary(lm(y ~ x + z, data = simData1)) # Looks good

write.csv(simData1, file = "../data/exampleData1.csv", row.names = FALSE)

dat1 <- readRDS(paste0(dataDir, "adamsKlpsData.rds"))

write.csv(dat1,
          paste0(dataDir, "adamsKlpsData.csv"),
          row.names = FALSE)
write.table(dat1,
            paste0(dataDir, "adamsKlpsData.txt"),
            row.names = FALSE,
            col.names = TRUE,
            sep = "\t")

