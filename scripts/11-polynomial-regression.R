##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(dplyr)
library(ggplot2)
library(readr)



##################################################
### Import and prepare data
##################################################

bluegills = read_csv("~/Dropbox/epsy-8264/data/bluegills.csv")
head(bluegills)



##################################################
### Examine linearity
##################################################

# Scatterplot of length versus age
ggplot(data = bluegills, aes(x = age, y = length)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Age") +
  ylab("Length") +
  theme_bw()


# Fit linear model
lm.1 = lm(length ~ 1 + age, data = bluegills)


# Examine residuals
augment(lm.1) %>% 
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  #geom_smooth(se = FALSE) +
  xlab("Fitted values") +
  ylab("Standardized residuals") +
  theme_bw()



##################################################
### Three methods to fit a cubic model
##################################################

# METHOD 1
# Create the quadratic and cubic terms in the data

bluegills = bluegills %>%
  mutate(
    age_quad = age ^ 2,
    age_cubic = age ^ 3
  )

head(bluegills)

# Fit models using terms
lm.3 = lm(length ~ 1 + age + age_quad + age_cubic, data = bluegills)

tidy(lm.3)
glance(lm.3)


# METHOD 2
# Create Polynomial Terms in the lm() Function

lm.3 = lm(length ~ 1 + age + I(age^2) + I(age^3), data = bluegills)

tidy(lm.3)
glance(lm.3)


# METHOD 3
# Use poly() Function in the lm() Function

lm.3 = lm(length ~ 1 + poly(age, 3, raw = TRUE), data = bluegills)

tidy(lm.3)
glance(lm.3)



##################################################
### Add cubic polynomial to ggplot
##################################################

ggplot(data = bluegills, aes(x = age, y = length)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~poly(x, 3, raw = TRUE), se = FALSE) +
  theme_bw() 



##################################################
### Fit all models up to the saturated model
##################################################

lm.1 = lm(length ~ 1 + age,                                             data = bluegills)
lm.2 = lm(length ~ 1 + age + I(age^2),                                  data = bluegills)
lm.3 = lm(length ~ 1 + age + I(age^2) + I(age^3),                       data = bluegills)
lm.4 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4),            data = bluegills)
lm.5 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5), data = bluegills)



##################################################
### Nested F-tests
##################################################

# Compare linear to quadratic model
anova(lm.1, lm.2)


# Compare models in sequence
anova(lm.1, lm.2, lm.3, lm.4, lm.5)



##################################################
### Examine residuals and influence for quadratic model
##################################################

# Plot of studentized residuals versus fitted values
residualPlot(lm.2)


# QQ-plot of the studentized residuals
qqPlot(lm.2, id = FALSE)


# Influence plot: Studentized residuals versus leverage; sized by Cook's D
influencePlot(lm.2)



##################################################
### Model output
##################################################

# Model-level information
glance(lm.2)


# Coefficient-level information
tidy(lm.2)


# Plot of the fitted model for interpretation
data.frame(
  age = seq(from = 1, to = 6, by = 0.1) #Set up sequence of x-values
  ) %>%
  mutate(
    yhat = predict(lm.2, newdata = .) #Get y-hat values based on model
  ) %>%
  ggplot(aes(x = age, y = yhat)) +
    geom_line() +
    theme_bw() +
    xlab("Age") +
    ylab("Predicted length")



