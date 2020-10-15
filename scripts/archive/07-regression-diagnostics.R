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

# Load data
davis = read_csv("~/Dropbox/epsy-8264/data/davis.csv")

# View data
head(davis)


# Add female dummy variable
davis = davis %>%
  mutate(
    female = if_else(sex == "F", 1, 0)
  )



##################################################
### Can we use measured weight and sex to predict reported weight?
##################################################

# Examine relationship
ggplot(data = davis, aes(x = weight, y = repwt, group = sex, color = sex)) +
  geom_text(aes(label = sex)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Measured weight (in kg)") +
  ylab("Reported weight (in kg)") +
  ggsci::scale_color_d3() +
  guides(color = FALSE)


# Fit interaction model
lm.1 = lm(repwt ~ 1 + weight + female + weight:female, data = davis)


# Model-level information
glance(lm.1) #Model-level inference
anova(lm.1)  #ANOVA decomposition


# Coefficient-level information
tidy(lm.1)


# Confidence intervals for parameters
confint(lm.1)



##################################################
### Use fixed data
##################################################

# Load data
davis_fixed = read_csv("~/Dropbox/epsy-8264/data/davis-corrected.csv") %>%
  mutate(
    female = if_else(sex == "F", 1, 0)
  )

# Examine relationship
ggplot(data = davis_fixed, aes(x = weight, y = repwt, group = sex, color = sex)) +
  geom_text(aes(label = sex)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Measured weight (in kg)") +
  ylab("Reported weight (in kg)") +
  ggsci::scale_color_d3() +
  guides(color = FALSE)

# Model-level information
lm.1_1 = lm(repwt ~ 1 + weight + female + weight:female, data = davis_fixed)

glance(lm.1_1) #Model-level inference
anova(lm.1_1)  #ANOVA decomposition
tidy(lm.1_1) # Coefficient-level information
confint(lm.1_1) # Confidence intervals for parameters


##################################################
### Leverage values
##################################################

# Obtaining leverage values
out_1 = augment(lm.1)
head(out_1)


# Average leverage value (h-bar)
mean(out_1$.hat)


# Find large leverage values: h >= 2(h-bar)
out_1 %>% 
  filter(.hat >= 2 * 0.02185792) %>% 
  arrange(desc(.hat))


# Index plot of the leverage values
ggplot(data = out_1, aes(x = as.integer(.rownames), y = .hat)) +
  geom_text(aes(label = .rownames), size = 3) +
  geom_hline(yintercept = 2 * 0.02185792, linetype = "dashed", color = "red") +
  theme_bw() +
  xlab("Observation number") +
  ylab("Leverage value (h)")



##################################################
### Regression outliers
##################################################

# Show 12th observations output from augment()
out_1[12, ]

# Compute studentized residual for observation 12 (by hand)
-29.22301 / (2.249039 * sqrt(1 - 0.7141856))


# Compute standardized residuals
rstandard(lm.1)
rstandard(lm.1)[12]

# Compute studentized residuals
rstudent(lm.1)
rstudent(lm.1)[12]



# Add studentized residuals to augmented data
out_1 = out_1 %>%
  mutate(
    student_resid = rstudent(lm.1)
  )


# Index plot of the studentized residuals
ggplot(data = out_1, aes(x = as.integer(.rownames), y = student_resid)) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  xlab("Observation number") +
  ylab("Studentized residuals")



##################################################
### Studentized residuals: Mean-shift model
##################################################

davis_2 = davis %>%
  mutate(
    obs_12 = if_else(row_number() == 12, 1, 0)
  )


lm.mean_shift = lm(repwt ~ 1 + weight + female + weight:female + obs_12, data = davis_2)
tidy(lm.mean_shift)



##################################################
### Test of the studentized residual = 0
##################################################

# Compute p-value
2 * pt(-24.30, df = 179)


# Adjust for the number of tests
(1.402353 * 10^-58) * 183


# Test of the largest absolute studentized residual = 0
outlierTest(lm.1)



##################################################
### Influence: DFBETAs
##################################################

dfbeta(lm.1)


# DFBETA for observation 12
dfbeta(lm.1)[12, ]


# Scaled DFBETA (using RMSE)
dfbetas(lm.1)

# Scaled DFBETA for observation 12
dfbetas(lm.1)[12, ]


# Add scaled DFBETA values to the augmented data
out_1 = out_1 %>%
  mutate(
    b_0           = dfbetas(lm.1)[ , 1],
    b_weight      = dfbetas(lm.1)[ , 2],
    b_female      = dfbetas(lm.1)[ , 3],
    b_interaction = dfbetas(lm.1)[ , 4]
    )

head(out_1)


# Index plot of the scaled DFBETA values (for the interaction term)
ggplot(data = out_1, aes(x = as.integer(.rownames), y = abs(b_interaction))) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  ggtitle("Weight x Female Interaction") +
  xlab("Observation number") +
  ylab("|Scaled DFBETA|")



##################################################
### Influence: Cook's Distance
##################################################

# Cook's D for observation 12
out_1[12, ]


# Index plot of the Cook's D values
ggplot(data = out_1, aes(x = as.integer(.rownames), y = .cooksd)) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  xlab("Observation number") +
  ylab("Cook's D")



##################################################
### Influence: DFFITS
##################################################

dffits(lm.1)


# DFFITS for observation 12
dffits(lm.1)[12]


# Add DFFITS to augmented data
out_1 = out_1 %>%
  mutate(
    DFFITS = dffits(lm.1)
  )


# Index plot of the absolute values of DFFITS
ggplot(data = out_1, aes(x = as.integer(.rownames), y = abs(DFFITS))) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  xlab("Observation number") +
  ylab("|DFFITS|")



##################################################
### Two alternative graphical plots
##################################################

# Plot of studentized residuals versus leverage values
ggplot(data = out_1, aes(x = student_resid, y = .hat)) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Leverage values")


# Plot of studentized residuals versus leverage values;
# Size observations according to Cook's D
ggplot(data = out_1, aes(x = student_resid, y = .hat)) +
  geom_text(aes(label = .rownames, size = .cooksd)) +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Leverage values")



##################################################
### Influence on SEs: COVRATIO
##################################################

covratio(lm.1)


# COVRATIO for observation 12
covratio(lm.1)[12]


# Add COVRATIOS to augmented data
out_1 = out_1 %>%
  mutate(
    COVRATIO = covratio(lm.1)
  )


# Index plot of the studentized residuals
ggplot(data = out_1, aes(x = as.integer(.rownames), y = COVRATIO)) +
  geom_text(aes(label = .rownames), size = 3) +
  theme_bw() +
  xlab("Observation number") +
  ylab("COVRATIO")



##################################################
### Measuring joint influence: Added variable plots
##################################################

# Display an added variable plot for weight
avPlot(lm.1, variable = "weight")


# Display an added variable plot for all coefficients
avPlots(lm.1, intercept = TRUE)



##################################################
### Re-fit model, removing observation 12 and 
##################################################

# Fit interaction model
lm.1 = lm(repwt ~ 1 + weight + female + weight:female, data = davis, subset = -c(12))



##################################################
### Quick diagnostic plots
##################################################

# a plot of residuals against fitted values, 
# a Scale-Location plot of sqrt(| residuals |) against fitted values, 
# a Normal Q-Q plot, 
# a plot of residuals against leverages
plot(lm.1)


# Show all four plots at once
par(mfrow = c(2, 2)) # set plot to a 2x2 display
plot(lm.1)
par(mfrow = c(1, 1)) # return plot to normal 1x1 display


# Index plots for several diagnostics
influenceIndexPlot(lm.1)

