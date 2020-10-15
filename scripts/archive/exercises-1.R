# Nonparametric Regression Exercises

# 1  Scatterplot Smoothing

# (a) Robey data on fertility and contraception

library(car)
data(Robey)
attach(Robey)

plot(contraceptors, tfr)
abline(lm(tfr ~ contraceptors), lty=2)
lines(loess.smooth(contraceptors, tfr, span=.5, family="gaussian"))

# (b)  SLID data

detach(Robey)
data(SLID)
attach(SLID)

hist(wages)
hist(education)
hist(age)

plot(education, wages)
lines(loess.smooth(education, wages, span=.3, family="gaussian"))
abline(lm(wages ~ education), lty=2)

plot(education^3, log(wages))
lines(loess.smooth(education^3, log(wages), span=.3, family="gaussian"))
abline(lm(log(wages) ~ I(education^3)), lty=2)

# (c) U.S. education data

detach(SLID)
data(States)
attach(States)

pairs(cbind(SATM, dollars, percent))

plot(dollars, SATM)
lines(loess.smooth(dollars, SATM, span=.7, family="gaussian"))
abline(lm(SATM ~ dollars), lty=2)

plot(percent, SATM)
lines(loess.smooth(percent, SATM, span=.5, family="gaussian"))
abline(lm(SATM ~ percent), lty=2) 

plot(dollars, percent)
lines(loess.smooth(dollars, percent, span=.7, family="gaussian"))
abline(lm(percent ~ dollars), lty=2)

mod.SATM <- lm(SATM ~ dollars + percent)
summary(mod.SATM)

cr.plots(mod.SATM)

mod.SATM.2 <- lm(SATM ~ dollars + log(percent/(100 - percent)))
summary(mod.SATM.2)
cr.plots(mod.SATM.2)
