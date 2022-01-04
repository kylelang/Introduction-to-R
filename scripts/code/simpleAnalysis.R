### Title:    Examples 4: Working with Data & Simple Analyses
### Author:   Kyle M. Lang
### Created:  2016-JAN-27
### Modified: 2016-JAN-29

rm(list = ls(all = TRUE))

library(psych)
library(multcomp)

dataDir <- "../data/"

dat1 <- readRDS(paste0(dataDir, "adamsKlpsData.rds"))
dat1 <- read.csv(paste0(dataDir, "adamsKlpsData.csv"))
dat1 <- read.table(paste0(dataDir, "adamsKlpsData.txt"),
                   header = TRUE,
                   sep = "\t")

## Look at the top of the dataset:
head(dat1)

##### COMPUTE SCALE SCORES #####

sysRacNames <- c(paste0("RIAE", c(2, 3, 7, 8, 9, 11, 12)),
                 paste0("NORI", c(2, 7, 9))
                 )

indRacNames <- c(paste0("RIAE", c(1, 4, 5, 6, 10)),
                 paste0("NORI", c(1, 4, 10))
                 )

policyNames <- paste0("POLICY", c(1, 3, 4, 5, 6))

sysRac <- rowMeans(dat1[ , sysRacNames], na.rm = TRUE)
indRac <- rowMeans(dat1[ , indRacNames], na.rm = TRUE)
policy <- rowMeans(dat1[ , policyNames], na.rm = TRUE)
polAffil <- dat1[ , "POLV"]
revDisc <- dat1[ , "POLICY2"]

dat2 <- data.frame(sysRac, indRac, policy, polAffil, revDisc)

saveRDS(dat2, paste0(dataDir, "adamsKlpsScaleScore.rds"))
write.csv(dat2,
          paste0(dataDir, "adamsKlpsScaleScore.csv"),
          row.names = FALSE)
write.table(dat2,
            paste0(dataDir, "adamsKlpsScaleScore.txt"),
            row.names = FALSE,
            col.names = TRUE,
            sep = "\t")


#### COMPUTE DESCRIPTIVE STATS #####


## Calculate the correlation matrix
cor(dat2)
cor(dat2, method = "spearman")# use Spearmans rho

## Calculate covariance matrix
cov(dat2)

## Compute variable means:
colMeans(dat2)

## Compute variable medians
apply(dat2, 2, median)

## Compute variable modes:
apply(dat2, 2,
      FUN = function(x) names( table(x) )[which.max( table(x) )]
      )

## Compute variable SDs:
apply(dat2, 2, sd)

## Compute variable Variances:
apply(dat2, 2, var)

## Compute Cronbach's Alpha for each scale:
alpha(x = dat1[ , sysRacNames])
alpha(x = dat1[ , indRacNames])
alpha(x = dat1[ , policyNames])

## We can also get bootstrapped CIs for the internal consistency estimates
alpha(x = dat1[ , sysRacNames], n.iter = 1000)
alpha(x = dat1[ , indRacNames], n.iter = 1000)
alpha(x = dat1[ , policyNames], n.iter = 1000)

## We can streamline this process with lapply:
datList <- list(sysRac = dat1[ , sysRacNames],
                indRac = dat1[ , indRacNames],
                policy = dat1[ , policyNames])

alphaList <- lapply(datList, alpha)

alphaList$policy
alphaList$sysRac
alphaList$indRac

## Check Skew & Kurtosis
skewVec <- apply(dat2, 2, skew)
kurtVec <- apply(dat2, 2, kurtosi)

skewVec
kurtVec

## Any really problematic varibles?
any(abs(skewVec) > 1.0)
any(abs(kurtVec) > 7.0)


##### SIMPLE INFERENTIAL ANALYSES #####


### Test of Bivariate Mean Differences:

## Is there differential endoresment of systemic and individual racism?
t.test(dat2$sysRac, dat2$indRac, paired = TRUE)

## Yes, there is significantly higher endorsement of individualistic
## definitions of racism.


### Multiple Linear Regression:


## After controlling for political affiliation, does definition of racism
## significantly predict belief in reverse discrimination?

mod1 <- lm(revDisc ~ polAffil, data = dat2)
summary(mod1)

## Political affiliation is a significant predictor of belief in reverse
## discrimination such that self-identifying as more conservative is
## positively associated with greater belief in reverse discrimination.

mod2 <- lm(revDisc ~ polAffil + indRac + sysRac, data = dat2)
anova(mod1, mod2)

## After controlling for political affiliation, there is no residual
## effect of definition of racism on beliefs in reverse discrimination.

## Maybe political affiliation is moderating the effect of
## definitions of racism on beliefs in reverse discimination?

mod3 <- lm(revDisc ~ polAffil*indRac + polAffil*sysRac, data = dat2)
summary(mod3)

## Not in these data.


### Simple ANOVA:


## Does type of feed affect dragon growth?
dragonFeed <- readRDS(paste0(dataDir, "dragonFeed.rds"))

## Check the basic data stats:
summary(dragonFeed)

## Look at the within-cell means:
with(dragonFeed, tapply(growth, diet, mean))

## Look at the within-cell SDs:
with(dragonFeed, tapply(growth, diet, mean))

## Fit a basic model:
anovaOut1 <- aov(growth ~ diet, data = dragonFeed)
summary(anovaOut1) # Basic ANOVA table
summary.lm(anovaOut1) # Summary of regression representation

### Let's look at some contrasts

## R will use the first level as the default reference category:
levels(dragonFeed$diet)

## See which feed types outperform the overall mean growth:
newCt <- contr.sum(levels(dragonFeed$diet))
contrasts(dragonFeed$diet) <- newCt

anovaOut2 <- aov(growth ~ diet, data = dragonFeed)
summary.lm(anovaOut2)

## Need to change the category ordering to see
## the contast for 'pelican'
dragonFeed$diet <- relevel(dragonFeed$diet, ref = "pelican")
newCt <- contr.sum(levels(dragonFeed$diet))
newCt
contrasts(dragonFeed$diet) <- newCt

anovaOut3 <- aov(growth ~ diet, data = dragonFeed)
summary.lm(anovaOut3)

## Looks like Aardvark is the only feed-creature type that
## produces growth rates significantly above the mean

## We can look at all pairwise comparisons, too:
tukeyOut1 <- glht(anovaOut1, linfct = mcp(diet = "Tukey"))
summary(tukeyOut1)
