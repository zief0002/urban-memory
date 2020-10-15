##--------------------------------------------##
##  Introduction to Nonparametric Regression  ##
##                John Fox                    ##
##        ESRC Oxford Spring School           ##
##                May 2005                    ##
##--------------------------------------------##

# nonparametric simple regression

    # local polynomial regression

        # UN data
            
library(car)
data(UN)
head(UN, 10)

UN <- na.omit(UN) # not strictly necessary
head(UN, 10)

attach(UN)
search()

plot(gdp, infant.mortality)
identify(gdp, infant.mortality, rownames(UN))

lines(loess.smooth(gdp, infant.mortality, span=.5, family="gaussian"), lwd=3)
abline(lm(infant.mortality ~ gdp), lty=2, lwd=3)

        # transforming to approximate linearity
            
plot(log10(gdp), log10(infant.mortality))
identify(log10(gdp), log10(infant.mortality), rownames(UN))
lines(loess.smooth(log10(gdp), log10(infant.mortality), span=.5,
    family="gaussian"), lwd=3)
abline(lm(log10(infant.mortality) ~ log10(gdp)), lty=2, lwd=3)

        # robust fit
            
plot(log10(gdp), log10(infant.mortality))
lines(loess.smooth(log10(gdp), log10(infant.mortality), span=.4,
    family="gaussian"), lwd=2, lty=2) # non-robust fit
lines(loess.smooth(log10(gdp), log10(infant.mortality), span=.4), 
    lwd=2) # robust fit
    
        # Canadian occupational prestige data
        
detach(UN)
rm(list=objects())
data(Prestige)
some(Prestige)
attach(Prestige)
search()

        # selecting the span
        
par(mfrow=c(2,3)) # 2 x 3 array of plots (or turn on plot history)
for (span in seq(.1, .9, by=.2)){
    plot(income, prestige, main=paste("s =", span))
    lines(lowess(income, prestige, f=span, iter=0), lwd=3)
    }

        # statistical inference

library(gam)

            # test of nonlinearity
            
mod.linear <- lm(prestige ~ income)
summary(mod.linear)

mod.nonpar <- loess(prestige ~ income, span=.6, degree=1)
summary(mod.nonpar)

RSS.1 <- sum(residuals(mod.linear)^2)
RSS.2 <- sum(residuals(mod.nonpar)^2)

f <- ((RSS.1 - RSS.2)/(4.3 - 2)) / (RSS.2/(102 - 4.3))
pf(f, 4.3 - 3, 102 - 4.3, lower.tail=FALSE)

            # test of income effect
            
TSS <- sum((prestige - mean(prestige))^2)
f <- ((TSS - RSS.2)/(4.3 - 1)) / (RSS.2/(102 - 4.3))
pf(f, 4.3 - 1, 102 - 4.3, lower.tail=FALSE)

            # confidence bands
            
inc <- seq(min(income), max(income), length=500)
fit <- predict(mod.nonpar, data.frame(income=inc), se=TRUE)
str(fit)

plot(income, prestige, ylim=c(15,95))
lines(inc, fit$fit, lwd=3)
lines(inc, fit$fit + 2*fit$se.fit, lty=2, lwd=2)
lines(inc, fit$fit - 2*fit$se.fit, lty=2, lwd=2)

        # regression and smoothing splines

            # regression spline
        
library(splines)
plot(income, prestige)
mod.regspline <- lm(prestige ~ ns(income, knots=10000))
fit <- predict(mod.regspline, data.frame(income=inc))
lines(inc, fit, lwd=3)
lines(loess.smooth(income, prestige, span=.6), lty=2, lwd=2) # cf.

            # smoothing spline
            
plot(income, prestige)
lines(smooth.spline(income, prestige, df=4.3), lwd=3)
lines(loess.smooth(income, prestige, span=.6), lty=2, lwd=2) # cf.

    # nonlinearity diagnostics: component+residual plots
    
mod.prestige <- lm(prestige ~ income + education, data=Prestige)
cr.plots(mod.prestige)


# nonparametric multiple regression

    # general model

mod.loess <- loess(prestige ~ income*education, span=.5, degree=1)
summary(mod.loess)

        # 3D perspective plot of fitted surface
    
new.prestige <- expand.grid(
    income = inc <- seq(0, 25000, length=20),
    education = ed <- seq(6, 16, length=20)
    )
fit <- predict(mod.loess, new.prestige)
graph <- persp(x=inc, y=ed, fit,
    xlab="Income", ylab="Education", zlab="Prestige", expand=0.5,
    col="gray70", ticktype="detailed", theta=-45)

trans3d <- function(x,y,z, pmat) {  # this function from the persp() help file
  tr <- cbind(x,y,z,1) %*% pmat
  list(x = tr[,1]/tr[,4], y= tr[,2]/tr[,4])
  }

points(trans3d(x=income, y=education, z=prestige, pmat=graph), col=2, pch=16)

        # contour plot of fitted surface
    
contour(x=inc, y=ed, fit, xlab="Income", ylab="Education",
    levels=seq(20, 80, by=10), labcex=1.2)
abline(h=seq(6, 16, by=2), v=seq(0, 25000, by=5000), col="gray80")

        # tests for terms in the model
    
mod.loess.inc <- loess(prestige ~ income, span=.7, degree=1)
summary(mod.loess.inc)
anova(mod.loess.inc, mod.loess) # test of education effect

mod.loess.ed <- loess(prestige ~ education, span=.7, degree=1)
summary(mod.loess.ed)
anova(mod.loess.ed, mod.loess) # test of income effect

    # additive regression model

library(gam)
prestige.additive <- gam(prestige ~ lo(income, span=.7) +
    lo(education, span=.7))
summary(prestige.additive)
par(mfrow=c(1,2))
plot(prestige.additive, ylab="Prestige", residuals=TRUE, se=TRUE, ask=FALSE)

        # inference (note: problem with df in gam package)
        

            # tests for terms in the model
            
prestige.inc <- gam(prestige ~ lo(income, span=.7))
prestige.ed <- gam(prestige ~ lo(education, span=.7))
anova(prestige.inc, prestige.additive, test="F") # test of education effect
anova(prestige.ed, prestige.additive, test="F") # test of income effect

            # test for interaction between income and education
            
prestige.general <- gam(prestige ~ lo(income, education, span=.5))
anova(prestige.additive, prestige.general, test="F")

            # linearity tests
            
prestige.inc.lin <- gam(prestige ~ income + lo(education, span=.7))
prestige.ed.lin <- gam(prestige ~ lo(income, span=.7) + education)
anova(prestige.inc.lin, prestige.additive)  # for income
anova(prestige.ed.lin, prestige.additive)  # for education

        # using the mgcv package
        
detach("package:gam")
library(mgcv)

prestige.additive.mgcv <- gam(prestige ~ s(income) + s(education))
summary(prestige.additive.mgcv)
par(mfrow=c(1,2))
plot(prestige.additive.mgcv, ylab="Prestige", residuals=TRUE, pch=1)

            # fixing the df for the smooth terms
            
prestige.additive.fixdf <- gam(prestige ~ s(income, fx=TRUE, k=4)
    + s(education, fx=TRUE, k=4))
summary(prestige.additive.fixdf)
par(mfrow=c(1,2))
plot(prestige.additive.fixdf, ylab="Prestige", residuals=TRUE, pch=1)

# generalized nonparametric regression

    # scatterplot smoothing

detach(Prestige)
rm(list=objects())
data(Mroz)
attach(Mroz)
some(Mroz)

par(mfrow=c(1,1))
plot(lwg, jitter(as.numeric(lfp == "yes"), factor=.125),
    xlab="Log Estimated Wages", ylab="Labour-Force Participation")
abline(h=c(0,1))
mod.lin.logit <- glm(lfp ~ lwg, family=binomial)
x <- seq(-2, 3, length=500)
lines(x, predict(mod.lin.logit, list(lwg=x), type="response"),
    lty=2, lwd=3)
mod.nonpar.logit <- gam(lfp ~ s(lwg), family=binomial)
lines(x, predict(mod.nonpar.logit, list(lwg=x), type="response"), lwd=3)

    # generalized additive model

table(k5)
table(k618)

mod.mroz.glm <- gam(lfp ~ age + inc + k5 +  # fits the glm via gam()
    + k618 + wc + hc, family=binomial)      # cf., same model fit with glm()

mod.mroz.gam <- gam(lfp ~ s(age) + s(inc) + factor(k5)
    + factor(k618) + wc + hc, family=binomial)
summary(mod.mroz.gam)
plot(mod.mroz.gam, residuals=TRUE, all.terms=TRUE, pch=1)

        # tests of nonlinearity
        
             # overall test of nonlinearity
        
anova(mod.mroz.glm, mod.mroz.gam, test="Chisq")

            # tests for nonlinearity of individual terms (e.g.)
            
mod.mroz.gam.age.lin <- gam(lfp ~ age + s(inc) + factor(k5)
    + factor(k618) + wc + hc, family=binomial)
anova(mod.mroz.gam.age.lin, mod.mroz.gam,
    test="Chisq") # test for linearity of age
    
mod.mroz.gam.k5.lin <- gam(lfp ~ s(age) + s(inc) + k5
    + factor(k618) + wc + hc, family=binomial)
anova(mod.mroz.gam.k5.lin, mod.mroz.gam,
    test="Chisq") # test for linearity of k5; note df != 2 exactly (but close)
    
        # tests for individual terms (e.g.)
        
mod.mroz.gam.minus.age <- gam(lfp ~ s(inc) + factor(k5)
    + factor(k618) + wc + hc, family=binomial)
anova(mod.mroz.gam.minus.age, mod.mroz.gam, test="Chisq") # test for age

mod.mroz.gam.minus.k5 <- gam(lfp ~ s(age) + s(inc)
    + factor(k618) + wc + hc, family=binomial)
anova(mod.mroz.gam.minus.k5, mod.mroz.gam, test="Chisq") # test for k5







