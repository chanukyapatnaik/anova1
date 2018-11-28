rm(list=ls())
setwd("/home/marc/School/ANOVA/Seminar_2")

## SocialAnxiety: Two-way unbalanced design
library(MASS)
library(car)
library(multcomp)

socanx.dat <- read.csv("SocialAnxiety.csv")

## Some esthetic changes
levels(socanx.dat$group) <- c("nosc_dep", "hlt", "socanx_dep",
                              "socanx_nodep")

## One value missing for sex variable
socanx.dat$sex[which(socanx.dat$sex == "")]  <- NA
socanx.dat$sex <- factor(socanx.dat$sex)

levels(socanx.dat$sex) <- c("f", "m")

colnames(socanx.dat) <- c("group", "sex", "socanx")

## Complete cases analysis
socanx.dat <- socanx.dat[complete.cases(socanx.dat), ]
str(socanx.dat)
rownames(socanx.dat) <- NULL


## Hypothesis testing (Contrasts)
socanx.lm1 <- lm(socanx ~ -1 + group:sex, data=socanx.dat)

group.cmat <- rbind(c(1, -1, 0, 0, 1, -1, 0, 0),
                    c(1, 0, -1, 0, 1, 0, -1, 0),
                    c(1, 0, 0, -1, 1, 0, 0, -1))

group.ctest <- glht(socanx.lm1, linfct=group.cmat)
summary(group.ctest, test=Ftest())

## Test against full model
## This doesn't work, I would have to specify the contrasts
## manually
contrasts(socanx.dat$group) <- "contr.sum"
contrasts(socanx.dat$sex) <- "contr.sum"

socanx.rlm1 <- aov(socanx ~ sex + group:sex, data=socanx.dat)
socanx.flm1 <- aov(socanx ~ sex + group + group:sex, data=socanx.dat)

anova(socanx.flm1, socanx.rlm1, test="F")

## Type 3 with Anova from car makes it easy
Anova(socanx.flm1, type=3)

## Test against full model
## Make sure the contrasts are right (contr.sum for Type 3)


socanx.faov1 <- aov(socanx ~ group*sex, data=socanx.dat,
                  contrasts=list(group="contr.sum", sex="contr.sum"))
drop1(socanx.faov1, ~ ., test="F")

socanx.dat$treatment <- socanx.dat$group:socanx.dat$sex

## Outliers?


## Homogeneity of variance
## Studentized residuals 
plot(fitted(socanx.faov1), rstandard(socanx.faov1))
lines(lowess(rstandard(socanx.faov1) ~ fitted(socanx.faov1)),
      col="red")

## Outliers
## Studentized deleted residuals so the function don't pick up
## on outliers
plot(rstudent(socanx.faov1), yaxt="n", xlab="Index",
     ylab="Studentized del. residuals")
abline(h=2.5)
abline(h=-2.5)
axis(2, at=seq(-3, 3, .5))

which(abs(rstudent(socanx.faov1)) > 2.5)

## One residual, index=112 (Obs. 113 in the original dataset)
## It is barely above the threshold
socanx.dat[112, ]
tmp.sub <- subset(socanx.dat, group == "socanx_dep", sex="m")
mean(tmp.sub$socanx)

## It is way lower than the treatment average, but it is not so
## extreme to indicate a measurement mistake. Maybe the individual had
## a very good day.







