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


## Hypothesis testing (by linear hypotheses)
## This is just for fun

socanx.lm1 <- lm(socanx ~ -1 + group:sex, data=socanx.dat)

group.cmat <- rbind(c(1, -1, 0, 0, 1, -1, 0, 0),
                    c(1, 0, -1, 0, 1, 0, -1, 0),
                    c(1, 0, 0, -1, 1, 0, 0, -1))

group.ctest <- glht(socanx.lm1, linfct=group.cmat)
summary(group.ctest, test=Ftest())

int.cmat <- rbind(c(1, -1, 0, 0, -1, 1, 0, 0),
                  c(1, 0, -1, 0, -1, 0, 1, 0),
                  c(1, 0, 0, -1, -1, 0, 0, 1))

int.ctest <- glht(socanx.lm1, linfct=int.cmat)
summary(int.ctest, test=Ftest())

sex.cmat <- rbind(c(1, 1, 1, 1, -1, -1, -1, -1))

sex.ctest <- glht(socanx.lm1, linfct=sex.cmat)
summary(sex.ctest, test=Ftest())

## Contrast specification
contrasts(socanx.dat$group) <- "contr.sum"
contrasts(socanx.dat$sex) <- "contr.sum"

## ## Test against full model
## ## This doesn't work, I would have to specify the contrasts
## ## manually

## socanx.rlm1 <- aov(socanx ~ sex + group:sex, data=socanx.dat)
## socanx.flm1 <- aov(socanx ~ sex + group + group:sex, data=socanx.dat)

## anova(socanx.flm1, socanx.rlm1, test="F")

## Type 3 with Anova from car makes it easy
Anova(socanx.flm1, type=3)

## Test against full model
## Make sure the contrasts are right (contr.sum for Type 3)
socanx.faov1 <- aov(socanx ~ group*sex, data=socanx.dat,
                  contrasts=list(group="contr.sum", sex="contr.sum"))
drop1(socanx.faov1, ~ ., test="F")

## Treatment variable for various uses
socanx.dat$treatment <- socanx.dat$group:socanx.dat$sex

## Homogeneity of variance
## Studentized residuals 
plot(fitted(socanx.faov1), rstandard(socanx.faov1))
lines(lowess(rstandard(socanx.faov1) ~ fitted(socanx.faov1)),
      col="red")

plot(socanx.dat$treatment, rstandard(socanx.faov1))
plot(socanx.dat$group, rstandard(socanx.faov1))

## There does appear to be heteroscedasiticity between
## the groups. 

## Not so clear from the scatterplot, but
## The boxplots give evidence of heteroscedasticity


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

qqnorm(rstandard(socanx.faov1))
qqline(rstandard(socanx.faov1))

## Apparent deviation from normality. Short tails, low kurtosis (flat
## top).
## It is not so extreme however. See plots below for comparison
## with a n sample from a normal distribution
n <- nrow(socanx.dat)

par(mfrow=c(3, 3))
for(i in 1:9) {
plot(density(rnorm(n)), col="red")
lines(density(stdres(socanx.faov1)))
}

## Independence of residuals
res2 <- rstandard(socanx.faov1)[-1]
res1 <- rstandard(socanx.faov1)[-n]

plot(res1, res2)
abline(a=0, b=1, lty="dashed")

acf(ts(rstandard(socanx.faov1)))

cor(res2, res1)

## The ACF and time plots give no indication of time dependency among
## the residuals. Correlation is close to 0. If there were more remove
## observations (NA), this way of checking for dependency would be an
## issue.

## We now turn to formal testing on the residuals

## Homoscedasticity

## The Brown-Forsythe test is chosen because it doesn't need equal
## sample sizes and is robust to non-normality.
leveneTest(socanx.faov1)

## Rejection of the null hypothesis that the expected deviation values
## are equal between treatments. It's fair to say that the assumption
## of homoscedasticity cannot be made.

## Independence: check for residual autocorrelation at lag 1
durbinWatsonTest(socanx.faov1, alternative="two.sided",
                 data=socanx.dat)
## As expected, we don't find time dependency.

## Outlier
t.df <- socanx.faov1$df.residual

## It's a one tailed test
t.alpha <- 1-.05/n

abs.studres <- abs(rstudent(socanx.faov1))

## We don't detect any significant outlier.
## Although the Bonferroni correction might be
## conservative, obs. 112 wasn't so far from the boundary
## in the appropriate residual plot.
sum(pt(abs.studres, t.df) > t.alpha)

## Shapiro and Kolmogorov for normality
shapiro.test(residuals(socanx.faov1))

## Kolmogorov for normality
ks.test(residuals(socanx.faov1), "pnorm", alternative="two.sided")

## Shapiro doesn't reject normality, but Kolmogorov does. We should
## probably stay on the safe side.

## Remedial measures
## Wghted-least squares not appropriate as it assumes normality of the
## residuals.

## Check proportionality of variances, sd and means
socanx.dat$resids <- residuals(socanx.faov1)
means <- tapply(socanx.dat$socanx, socanx.dat$treatment, mean)
sds <- tapply(socanx.dat$resids, socanx.dat$treatment, sd)
vars <- tapply(socanx.dat$resids, socanx.dat$treatment, var)

cor(means, sds); cor(means, vars)
cor(means^2, sds)
## Can't find evidence of proportionality between the variances of the
## residuals and the treatment mean. There isn't an obvious
## transformation of Y in this case. 





























