##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(tidyverse)
library(patchwork)




##################################################
### Import and prepare data
##################################################

# Import data
contraception = read_csv(file = "~/Documents/github/epsy-8264/data/contraception.csv")


# Create dummy variable for GNI indicator and single letter variable
contraception = contraception %>%
  mutate(
    high_gni = if_else(gni == "High", 1, 0),
    gni2 = str_sub(contraception$gni, start = 1L, end = 1L)
  )


# View data
head(contraception)



##################################################
### Scatterplot
##################################################

ggplot(data = contraception, aes(x = educ_female, y = contraceptive, color = gni2)) +
  geom_text(aes(label = gni2)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Female education level") +
  ylab("Contraceptive rate") +
  ggsci::scale_color_d3() +
  guides(color = FALSE)



##################################################
### Fit interaction model
##################################################

# Fit interaction model
lm.1 = lm(contraceptive ~ 1 + educ_female + high_gni + educ_female:high_gni, data = contraception)


# Model-level information
glance(lm.1)


# Coefficient-level information
tidy(lm.1, conf.int = 0.95)



##################################################
### Omit three countries
##################################################

# Omit countries
contraception2 = contraception %>%
  filter(!country %in% c("Japan", "Maldives", "Tajikistan"))


# Examine relationship
ggplot(data = contraception2, aes(x = educ_female, y = contraceptive, color = gni2)) +
  geom_text(aes(label = gni2)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Female education level") +
  ylab("Contraceptive rate") +
  ggsci::scale_color_d3() +
  guides(color = FALSE)



# Fit model
lm.2 = lm(contraceptive ~ 1 + educ_female + high_gni + educ_female:high_gni, data = contraception2)

glance(lm.2) # Model-level information
tidy(lm.2, conf.int = 0.95) # Coefficient-level information



##################################################
### Leverage values
##################################################

# Augment model
out_1 = augment(lm.1)


# View augmented data
head(out_1)


# Average leverage value (h-bar)
mean(out_1$.hat)


# Find large leverage values: h >= 2(h-bar)
out_1 %>% 
  filter(.hat >= 2 * 0.04123711) %>% 
  arrange(desc(.hat))



##################################################
### Index plot of hat values (ID labeled)
##################################################

# Add country names and ID number to augmented data
out_1 = out_1 %>%
  mutate(
    id = as.integer(rownames(contraception)),
    country = contraception$country
  )


# Index plot of the leverage values
ggplot(data = out_1, aes(x = id, y = .hat)) +
  geom_text(aes(label = id), size = 3) +
  geom_hline(yintercept = 2 * 0.04123711, linetype = "dashed", color = "red") +
  theme_bw() +
  xlab("Observation number") +
  ylab("Leverage value (h)")



##################################################
### Index plot of hat values (Country name labeled)
##################################################

# Identify countries with high leverage
high_lev = out_1 %>% 
  filter(.hat >= 2 * 0.04123711)

# Identify countries with non-high leverage
low_lev = out_1 %>% 
  filter(.hat < 2 * 0.04123711)


# Index plot of the leverage values
ggplot(data = low_lev, aes(x = id, y = .hat)) +
  geom_text(aes(label = id), size = 3) +
  geom_text(data = high_lev, aes(label = country), size = 3) +
  geom_hline(yintercept = 2 * 0.04123711, linetype = "dashed", color = "red") +
  theme_bw() +
  xlab("Observation number") +
  ylab("Leverage value (h)")



##################################################
### Regression outliers
##################################################

# Show first observation's output from augment()
# As of 09-09-2020 broom was outputting the wrong sign!
out_1[1, ]

# Compute studentized residual for observation 1 (by hand)
1.34 / (14.5 * sqrt(1 - 0.0875))


# Compute raw residuals
resid(lm.1)
resid(lm.1)[1]


# Compute standardized residuals
rstandard(lm.1)
rstandard(lm.1)[1]


# Compute studentized residuals
rstudent(lm.1)
rstudent(lm.1)[1]



# Add studentized residuals to augmented data
out_1 = out_1 %>%
  mutate(
    student_resid = rstudent(lm.1)
  )




##################################################
### Plot of the studentized residuals vs. the fitted values
##################################################

ggplot(data = out_1, aes(x = .fitted, y = student_resid)) +
  geom_text(aes(label = id), size = 3) +
  theme_bw() +
  xlab("Observation number") +
  ylab("Studentized residuals")



##################################################
### Studentized residuals: Mean-shift model
##################################################

contraception_ms = contraception %>%
  mutate(
    obs_1 = if_else(row_number() == 1, 1, 0)
  )


lm.mean_shift = lm(contraceptive ~ 1 + educ_female + high_gni + educ_female:high_gni + obs_1, data = contraception_ms)
tidy(lm.mean_shift, conf.int = 0.95)



##################################################
### Test of the studentized residual = 0
##################################################

# Compute p-value
2 * (1 - pt(0.0970, df = 93))


# Adjust for the number of tests
0.9229351 * 97


# Test of the largest absolute studentized residual = 0
car::outlierTest(lm.1)



##################################################
### Influence: DFBETA and scaled DFBETA
##################################################

dfbeta(lm.1)


# DFBETA for observation 1
dfbeta(lm.1)[1, ]


# Scaled DFBETA (using RMSE)
dfbetas(lm.1)

# Scaled DFBETA for observation 1
dfbetas(lm.1)[1, ]



##################################################
### Index plots of scaled DFBETA values
##################################################

# Add scaled DFBETA values to the augmented data
out_1 = out_1 %>%
  mutate(
    b_0           = dfbetas(lm.1)[ , 1],
    b_educ_female = dfbetas(lm.1)[ , 2],
    b_gni         = dfbetas(lm.1)[ , 3],
    b_interaction = dfbetas(lm.1)[ , 4]
    )

head(out_1)


# Index plot of the scaled DFBETA values (for the interaction term)
p1 = ggplot(data = out_1, aes(x = id, y = abs(b_0))) +
  geom_text(aes(label = id), size = 3) +
  theme_bw() +
  ggtitle("Intercept") +
  xlab("Observation number") +
  ylab("|Scaled DFBETA|")

p2 = ggplot(data = out_1, aes(x = id, y = abs(b_educ_female))) +
  geom_text(aes(label = id), size = 3) +
  theme_bw() +
  ggtitle("Female Education") +
  xlab("Observation number") +
  ylab("|Scaled DFBETA|")

p3 = ggplot(data = out_1, aes(x = id, y = abs(b_gni))) +
  geom_text(aes(label = id), size = 3) +
  theme_bw() +
  ggtitle("GNI") +
  xlab("Observation number") +
  ylab("|Scaled DFBETA|")

p4 = ggplot(data = out_1, aes(x = id, y = abs(b_interaction))) +
  geom_text(aes(label = id), size = 3) +
  theme_bw() +
  ggtitle("Female Education x GNI") +
  xlab("Observation number") +
  ylab("|Scaled DFBETA|")


# Layout plots
(p1 | p2) / (p3 | p4)



##################################################
### Influence: Cook's Distance
##################################################

# Cook's D for observation 1
out_1[1, ]


# Index plot of the Cook's D values
ggplot(data = out_1, aes(x = id, y = .cooksd)) +
  geom_text(aes(label = id), size = 3) +
  theme_bw() +
  xlab("Observation number") +
  ylab("Cook's D")



##################################################
### Influence: DFFITS
##################################################

dffits(lm.1)


# DFFITS for observation 1
dffits(lm.1)[1]


# Add DFFITS to augmented data
out_1 = out_1 %>%
  mutate(
    DFFITS = dffits(lm.1)
  )


# Index plot of the absolute values of DFFITS
ggplot(data = out_1, aes(x = id, y = abs(DFFITS))) +
  geom_text(aes(label = id), size = 3) +
  theme_bw() +
  xlab("Observation number") +
  ylab("|DFFITS|")



##################################################
### Two alternative graphical plots
##################################################

# Plot of studentized residuals versus leverage values
ggplot(data = out_1, aes(x = student_resid, y = .hat)) +
  geom_text(aes(label = id), size = 3) +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Leverage values")


# Plot of studentized residuals versus leverage values;
# Size observations according to Cook's D
ggplot(data = out_1, aes(x = student_resid, y = .hat)) +
  geom_text(aes(label = id, size = .cooksd)) +
  theme_bw() +
  xlab("Studentized residuals") +
  ylab("Leverage values")



##################################################
### Influence on SEs: COVRATIO
##################################################

covratio(lm.1)


# COVRATIO for observation 1
covratio(lm.1)[1]


# Add COVRATIOS to augmented data
out_1 = out_1 %>%
  mutate(
    COVRATIO = covratio(lm.1)
  )


# Index plot of the studentized residuals
ggplot(data = out_1, aes(x = id, y = COVRATIO)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_text(aes(label = id), size = 3) +
  theme_bw() +
  xlab("Observation number") +
  ylab("COVRATIO")



##################################################
### Measuring joint influence: Added variable plots
##################################################

# Display an added variable plot for main effect of female education level
avPlot(lm.1, variable = "educ_female")


# Display an added variable plot for all coefficients
avPlots(lm.1, intercept = TRUE)



##################################################
### Re-fit model, removing observations 53 and 84
##################################################

# Fit interaction model
lm.1 = lm(contraceptive ~ 1 + educ_female + high_gni + educ_female:high_gni, data = contraception[-c(53,84), ])



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

