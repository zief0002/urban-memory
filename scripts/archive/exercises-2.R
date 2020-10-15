# Nonparametric Regression Exercises

# 2 Nonparametric Multiple Rergression

# (a) U.S. states education data

library(car)
data(States)
attach(States)
some(States)
library(mgcv)

mod.states.add <- gam(SATM ~ s(dollars) + s(percent))
summary(mod.states.add)
plot(mod.states.add, residuals=TRUE, pch=1)

mod.states.gen <- gam(SATM ~ s(dollars, percent, fx=TRUE, k=15))
anova(mod.states.add, mod.states.gen, test="F")

mod.states.add.1 <- gam(SATM ~ s(dollars, fx=TRUE, k=3) 
    + s(percent, fx=TRUE, k=5))  # refit fixing the df

    # tests for linearity
    
mod.states.dollars.lin <- gam(SATM ~ dollars + s(percent, fx=TRUE, k=5))
anova(mod.states.dollars.lin, mod.states.add.1, test="F")

mod.states.percent.lin <- gam(SATM ~ s(dollars, fx=TRUE, k=3) + percent)
anova(mod.states.percent.lin, mod.states.add.1, test="F")

    # tests for terms
    
mod.states.minus.dollars <- gam(SATM ~ s(percent, fx=TRUE, k=5))
anova(mod.states.minus.dollars, mod.states.add.1, test="F")

mod.states.minus.percent <- gam(SATM ~ s(dollars, fx=TRUE, k=5))
anova(mod.states.minus.percent, mod.states.add.1, test="F")

# (b) SLID data

detach(States)
data(SLID)
SLID <- na.omit(SLID)
attach(SLID)
some(SLID)

mod.slid <- gam(log(wages) ~ s(education) + s(age) + sex + language)
summary(mod.slid)
plot(mod.slid, residuals=TRUE)

mod.slid.1 <- gam(log(wages) ~ s(education, fx=TRUE, k=6) + s(age, fx=TRUE, k=9) 
    + sex + language)  # refitting fixing df

    # tests for linearity
        
mod.slid.ed.lin <- gam(log(wages) ~ education + s(age, fx=TRUE, k=9) 
    + sex + language)
anova(mod.slid.ed.lin, mod.slid.1, test="F")

mod.slid.age.lin <- gam(log(wages) ~ s(education, fx=TRUE, k=6) + age
    + sex + language)
anova(mod.slid.age.lin, mod.slid.1, test="F")

    # tests for terms
    
mod.slid.minus.ed <- gam(log(wages) ~ s(age, fx=TRUE, k=9) + sex + language)
anova(mod.slid.minus.ed, mod.slid.1, test="F")

mod.slid.minus.age <- gam(log(wages) ~ s(education, fx=TRUE, k=6) + sex + language)
anova(mod.slid.minus.age, mod.slid.1, test="F")

mod.slid.minus.sex <- gam(log(wages) ~ s(education, fx=TRUE, k=6) 
    + s(age, fx=TRUE, k=9) + language)
anova(mod.slid.minus.sex, mod.slid.1, test="F")
    
mod.slid.minus.language <- gam(log(wages) ~ s(education, fx=TRUE, k=6) 
    + s(age, fx=TRUE, k=9) + sex)
anova(mod.slid.minus.language, mod.slid.1, test="F")

# 3  Generalized Additive Models

# (a) Logistic regression, SLID data

detach(SLID)
data(SLID)
attach(SLID)
some(SLID)

SLID$working <- !is.na(SLID$wages)
SLID2 <- na.omit(SLID[, c("working", "education", "age", "sex")])
attach(SLID2)
some(SLID2)
table(working, age)

mod.working <- gam(working ~ s(education) + s(age) + sex, family=binomial,
    subset=age < 70) # note restriction to age < 70
summary(mod.working)
plot(mod.working, residuals=TRUE, ylab="working")

mod.working.1 <- gam(working ~ s(education, fx=TRUE, k=4) + s(age, fx=TRUE, k=3) 
 + sex, family=binomial, subset=age < 70)  # refitting fixing df

    # tests for terms
     
mod.working.minus.ed <- gam(working ~ s(age, fx=TRUE, k=3) 
 + sex, family=binomial, subset=age < 70)
anova(mod.working.minus.ed, mod.working.1, test="Chisq")
 
mod.working.minus.age <- gam(working ~ s(education, fx=TRUE, k=4)
 + sex, family=binomial, subset=age < 70)
anova(mod.working.minus.age, mod.working.1, test="Chisq")
 
mod.working.minus.sex <- gam(working ~ s(education, fx=TRUE, k=4) 
    + s(age, fx=TRUE, k=3), family=binomial, subset=age < 70)
anova(mod.working.minus.sex, mod.working.1, test="Chisq")

    # tests for linearity
    
mod.working.ed.lin <- gam(working ~ education + s(age, fx=TRUE, k=3) 
 + sex, family=binomial, subset=age < 70)
anova(mod.working.ed.lin, mod.working.1, test="Chisq")
 
mod.working.age.lin <- gam(working ~ s(education, fx=TRUE, k=4) + age
 + sex, family=binomial, subset=age < 70)
anova(mod.working.age.lin, mod.working.1, test="Chisq")

# (b) Logistic regression, SLID data, continued

mod.working.glm <- glm(working ~ (education + age)*sex, family=binomial,
    subset=age < 70)
summary(mod.working.glm)
Anova(mod.working.glm)

female <- sex == "Female"
male <- sex == "Male"

mod.working.gam <- gam(working ~ s(education, by=female, fx=TRUE, k=4)
    + s(age, by=female, fx=TRUE, k=4) 
    + s(education, by=male, fx=TRUE, k=4)
    + s(age, by=male, fx=TRUE, k=4) + sex, family=binomial, subset=age < 70)
plot(mod.working.gam, residuals=TRUE)

    # tests for interactions
        
mod.working.minus.edsex <- gam(working ~ s(education, fx=TRUE, k=4)
    + s(age, by=female, fx=TRUE, k=4) 
    + s(age, by=male, fx=TRUE, k=4) + sex, family=binomial, subset=age < 70)
anova(mod.working.minus.edsex, mod.working.gam, test="Chisq")

mod.working.minus.agesex <- gam(working ~ s(age, fx=TRUE, k=4)
    + s(education, by=female, fx=TRUE, k=4)
    + s(education, by=male, fx=TRUE, k=4)
    + sex, family=binomial, subset=age < 70)
anova(mod.working.minus.agesex, mod.working.gam, test="Chisq")

# (c) Poisson regression, Long's PhD productivity data

detach(SLID2)
Long <- read.table(
    "http://socserv.socsci.mcmaster.ca/jfox/Courses/Oxford/Long.txt",
    header=TRUE)
attach(Long)
some(Long)
mod.long <- gam(art ~ s(phd) + s(ment) + fem + mar + as.factor(kid5),
    family=poisson)
summary(mod.long)
plot(mod.long, residuals=TRUE, pch=1)

mod.long.1 <- gam(art ~ s(phd, fx=TRUE, k=9) + s(ment, fx=TRUE, k=9) 
    + fem + mar + as.factor(kid5), family=poisson) # refitting fixing df

    # tests for terms
    
mod.long.minus.phd <- gam(art ~ s(ment, fx=TRUE, k=9) 
    + fem + mar + as.factor(kid5), family=poisson)
anova(mod.long.minus.phd, mod.long.1, test="Chisq")
    
mod.long.minus.ment <- gam(art ~ s(phd, fx=TRUE, k=9) 
    + fem + mar + as.factor(kid5), family=poisson)
anova(mod.long.minus.ment, mod.long.1, test="Chisq")
    
mod.long.minus.fem <- gam(art ~ s(phd, fx=TRUE, k=9) + s(ment, fx=TRUE, k=9) 
    + mar + as.factor(kid5), family=poisson)
anova(mod.long.minus.fem, mod.long.1, test="Chisq")
    
mod.long.minus.mar <- gam(art ~ s(phd, fx=TRUE, k=9) + s(ment, fx=TRUE, k=9) 
    + fem + as.factor(kid5), family=poisson)
anova(mod.long.minus.mar, mod.long.1, test="Chisq")
    
mod.long.minus.kid5 <- gam(art ~ s(phd, fx=TRUE, k=9) + s(ment, fx=TRUE, k=9) 
    + fem + mar, family=poisson)
anova(mod.long.minus.kid5, mod.long.1, test="Chisq")

    # tests for linearity
    
mod.long.phd.lin <- gam(art ~ phd + s(ment, fx=TRUE, k=9) 
    + fem + mar + as.factor(kid5), family=poisson)
anova(mod.long.phd.lin, mod.long.1, test="Chisq")
    
mod.long.ment.lin <- gam(art ~ s(phd, fx=TRUE, k=9) + ment 
    + fem + mar + as.factor(kid5), family=poisson)
anova(mod.long.ment.lin, mod.long.1, test="Chisq")
 
mod.long.kid5.lin<- gam(art ~ s(phd, fx=TRUE, k=9) + s(ment, fx=TRUE, k=9) 
    + fem + mar + kid5, family=poisson)
anova(mod.long.kid5.lin, mod.long.1, test="Chisq")






