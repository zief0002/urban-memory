library(broom)
library(car)
library(dplyr)
library(ggplot2)
library(readr)


houston = read_csv("~/Dropbox/epsy-8264/data/houston.csv")
#duncan = read_csv("~/Dropbox/epsy-8264/data/duncan.csv")


ggplot(data = houston, aes(x = new_homes, y = med_ppsf)) +
  geom_point() +
  theme_bw() +
  xlab("Proportion of new homes sold in 2006") +
  ylab("Median price per square foot")

ggplot(data = houston, aes(x = foreclosed, y = med_ppsf)) +
  geom_point() +
  theme_bw() +
  xlab("Proportion of homes sold in 2006 that were foreclosed") +
  ylab("Median price per square foot")

ggplot(data = houston, aes(x = new_homes, y = foreclosed)) +
  geom_point() +
  theme_bw() +
  xlab("Proportion of homes sold in 2006") +
  ylab("Proportion of homes sold in 2006 that were foreclosed")

scatterplotMatrix(houston[c("med_ppsf", "new_homes", "foreclosed")])

lm.1 = lm(med_ppsf ~  1 + new_homes + foreclosed, data = houston)

residualPlot(lm.1)


# Log-transformation on Y
lm.2 = lm(log(med_ppsf) ~  1 + new_homes + foreclosed, data = houston)
residualPlot(lm.2)


lm.4 = lm(log(med_ppsf) ~  1 + I(foreclosed+1), data = houston, weights = 1/I(foreclosed + 1))
residualPlot(lm.4)


lm.4 = lm(log(med_ppsf) ~  1 + I(new_homes+1), data = houston, weights = 1/I(new_homes + 1))
residualPlot(lm.4)


out_1 = augment(lm.1)

lm.5 = lm(med_ppsf ~  1 + new_homes + foreclosed, data = houston, weights = (1/out_1$.sigma^2))
residualPlot(lm.5)


houston = houston %>%
  mutate(
    w_i = 1/out_1$.sigma^2,
    Y_w = sqrt(w_i) * med_ppsf,
    X1_w = sqrt(w_i) * new_homes,
    X2_w = sqrt(w_i) * foreclosed
  )


lm.5 = lm(Y_w ~  X1_w + X2_w-1, data = houston)
residualPlot(lm.5)

lm.5 = lm(Y_w ~  1 + X1_w + X2_w, data = houston)
residualPlot(lm.5)


#########

canada = read_csv("~/Dropbox/epsy-8264/data/canadian-prestige.csv")

canada2 = canada %>%
  select(wages, sex, age, education) %>%
  tidyr::drop_na()

slid = read.table("~/Desktop/SLID-Ontario.txt", header = TRUE)

slid = read_csv("~/Dropbox/epsy-8264/data/slid.csv")

lm.1 = lm(compositeHourlyWages ~  1 + sex + age + yearsEducation, data = slid)
residualPlot(lm.1)
tidy(lm.1)


lm.2 = lm(wages ~ 1 + age + education + male + age:education, data = slid)

# Examine residual plots
qqPlot(lm.2, id = FALSE)
residualPlot(lm.2)






out_1 = augment(lm.1) %>%
  mutate(
    abs_resid = abs(.resid),
    sq_resid = .resid ^ 2
    )

lm.stage2 = lm(abs_resid ~ 1 + .fitted + I(fitted(lm.w)^2), data = out_1)

w_i = 1 / (fitted(lm.stage2) ^ 2)


lm.w = lm(compositeHourlyWages ~  1 + age + yearsEducation, data = slid, weights = w_i)

plot(weighted.residuals(lm.w) ~ fitted(lm.w) + ))

residualPlot(lm.w)

out_1 = augment(lm.w) %>%
  mutate(w_resid = weighted.residuals(lm.w))


out_1 = augment(lm.1) %>%
  mutate(esr = rstudent(lm.1))

lm(abs(esr) ~ 1 + 1 + .fitted, data = out_1) %>% tidy()                 #t=17.8
lm(abs(esr) ~ 1 + 1 + I(1/.fitted), data = out_1) %>% tidy()            #t=-9.58
lm(abs(esr) ~ 1 + 1 + I(sqrt(abs(.fitted))), data = out_1) %>% tidy()   #t=17.5
lm(abs(esr) ~ 1 + 1 + I(1/sqrt(abs(.fitted))), data = out_1) %>% tidy() #t=-13.6






X = model.matrix(lm.1) 
V = 6.54^2 * diag(3997)





S[1,1]
  
nrow(X)


V_b = solve(t(X) %*% X) %*% t(X) %*% V %*% X %*% solve(t(X) %*% X)
sqrt(diag(V_b))

## White's adjustment to SEs

SIGMA = augment(lm.1)$.resid ^ 2  
S = SIGMA * diag(3997)

white_V_b = solve(t(X) %*% X) %*% t(X) %*% S %*% X %*% solve(t(X) %*% X)
sqrt(diag(white_V_b))


## White's modified adjustment to SEs

SIGMA_mod = augment(lm.1)$.resid ^ 2  / ((1 - augment(lm.1)$.hat)^2)
S = SIGMA_mod * diag(3997)

mod_white_V_b = solve(t(X) %*% X) %*% t(X) %*% S %*% X %*% solve(t(X) %*% X)
sqrt(diag(mod_white_V_b))


head(canada)


######

lm.1 = lm(Y ~ 1 + X1 + X2 + X3, data = p198)
tidy(lm.1)

residualPlot(lm.1)


# Omit Alaska
lm.1 = lm(Y ~ 1 + X1 + X2 + X3, data = p198, subset = -c(49))
tidy(lm.1)

residualPlot(lm.1)

slid %>%
  mutate(male = if_else(sex == "Male", 1, 0)) %>%
  write_csv("~/Desktop/slid.csv")

head(p198)
names(p198) = c("state", "education_exp", "income", "n_under_18", "n_urban", "region")
write_csv(p198, "~/Desktop/education-expenditures.csv")




evals = read_csv("~/Documents/github/epsy-8251/data/evaluations.csv")
