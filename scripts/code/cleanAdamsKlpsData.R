### Title:    R Mini Camp: Adams KLPS Data Prep
### Author:   Kyle M. Lang
### Created:  2015-OCT-04
### Modified: 2016-JAN-29

rm(list = ls(all = TRUE))

set.seed(235711)

dataDir <- "../data/"
source("quarkExampleSupportFunctions.R")

## Read the data:
tmp <- readLines(paste0(dataDir, "adamsklps_sem_data.txt"), -1)
tmp2 <- strsplit(tmp, "\t")
dat1 <- do.call("rbind", lapply(tmp2[-1], function(x) {x[1 : 33]}))
dat1 <- apply(dat1, 2, as.numeric)
colnames(dat1) <- tmp2[[1]][1 : 33]


## Simulate new data with similar moments to dat1:
samSize <- 1000

dat2 <- dat1
colnames(dat2) <- c(paste0("sysRac", c(1 : 10)),
                    paste0("indRac", c(1 : 8)),
                    paste0("policy", c(1 : 5)),
                    "revDisc",
                    paste0("equalOps", c(1 : 8)),
                    "polAffil")

policyFreq <- apply(dat2[ , grep("policy", colnames(dat2))], 2, table)

ordData <- matrix(NA, samSize, ncol(policyFreq))
colnames(ordData) <- paste0("policy", c(1 : 5))
for(i in 1 : ncol(policyFreq)) {
    ordData[ , i] <- apply(rmultinom(n = samSize,
                                     size = 1,
                                     prob = policyFreq[ , i]),
                           2,
                           FUN = function(x) { which(as.logical(x)) }
                           )   
}

tmpProbs <- colMeans(apply(dat2[ , c("revDisc", "polAffil")], 2,
                           FUN = function(x) {
                               as.numeric(cut(x, breaks = 2)) - 1
                           })
                     )

nomData <- sapply(tmpProbs,
                  FUN = function(x) {
                      rbinom(n = samSize, size = 1, prob = x)
                  })

colnames(nomData) <- c("revDisc", "polAffil")

normMeans <-
    colMeans(dat2[ , grep("sysRac|indRac", colnames(dat2))],
             na.rm = TRUE)
normSigma <-
    cov(dat2[ , grep("sysRac|indRac", colnames(dat2))],
        use = "pairwise")

normData <- rmvnorm(samSize, mean = normMeans, sigma = normSigma)

id <- c(1 : nrow(normData))
id2 <- paste0("suzy", id)

newData <- data.frame(id, id2, normData, nomData, ordData)

## Impose missing:
parms <- list()
parms$pm <- 0.25
parms$auxVars <- paste0("sysRac", c(1 : 3))
parms$incompVars <- setdiff(colnames(newData), parms$auxVars)
parms$marType <- sample(c("lower", "upper", "center", "tails"),
                        length(parms$incompVars),
                        replace = TRUE)

missData <- imposeMissing(newData, parms)

## Save the data:
saveRDS(newData, paste0(dataDir, "syntheticAdamsKlpsData.rds"))
saveRDS(missData, paste0(dataDir, "syntheticAdamsKlpsMissData.rds"))
