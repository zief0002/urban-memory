##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



##################################################
### Import and prepare data
##################################################

tokyo = read_csv("~/Dropbox/epsy-8264/data/tokyo-water-use.csv")
head(tokyo)



##################################################
### Scatterplot
##################################################

ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  scale_x_continuous(
    name = "Year", 
    breaks = seq(from = 1970, to = 2005, by = 5)
    ) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
    ) +
  theme_bw()



##################################################
### Polynomial model
##################################################

# Plot
ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  scale_x_continuous(
    name = "Year", 
    breaks = seq(from = 1970, to = 2005, by = 5)
  ) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
  ) +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 3))


# Fit cubic model
lm.cubic = lm(water_use ~ 1 + year + I(year^2) + I(year^3), data = tokyo)


# Examine residuals
augment(lm.cubic) %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals")



##################################################
### Create indicator variables from knots
##################################################

tokyo = tokyo %>%
  mutate(
    I_1 = if_else(year <= 1980, 0, year - 1980),
    I_2 = if_else(year <= 1992, 0, year - 1992)
  )

tokyo


##################################################
### Fit continuous piecewise model
##################################################

lm.pw = lm(water_use ~ 1 + year + I_1 + I_2, data = tokyo)
tidy(lm.pw)



##################################################
### Plot the fitted model
##################################################

# Create data set for plot
plot_data = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
  ) %>%
  mutate(
    I_1 = if_else(year <= 1980, 0, year - 1980),
    I_2 = if_else(year <= 1992, 0, year - 1992)
  )


# Add in fitted values
plot_data = plot_data %>%
  mutate(
    yhat = predict(lm.pw, newdata = plot_data)
  )


# Plot
ggplot(data = plot_data, aes(x = year, y = yhat)) +
  geom_line() +
  geom_point(data = tokyo, aes(x = year, y = water_use), alpha = 0.5) +
  theme_bw() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
    )



##################################################
### Graphical Inference
##################################################

# Create plot data with SEs
plot_data2 = plot_data %>%
  mutate(
    yhat = predict(lm.pw, newdata = plot_data, se.fit = TRUE)$fit,
    se   = predict(lm.pw, newdata = plot_data, se.fit = TRUE)$se.fit
  )

head(plot_data2)


# Add in lower- and upper-limits for CIs
plot_data2 = plot_data2 %>%
  mutate(
    lower_limit = yhat - 2*se,
    upper_limit = yhat + 2*se
  )


# Plot
ggplot(data = plot_data2, aes(x = year, y = yhat)) +
  geom_line() +
  geom_line(aes(x = year, y = lower_limit), color = "red", linetype = "dashed") +
  geom_line(aes(x = year, y = upper_limit), color = "red", linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
    )


# Plot (filled in SE)
ggplot(data = plot_data2, aes(x = year, y = yhat)) +
  geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit), color = "grey", alpha = 0.3) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
  )



##################################################
### Residuals
##################################################

# Augment the model
out_pw = augment(lm.pw)


# Check normality
sm.density(out_pw$.std.resid, model = "normal")


# Check other assumptions
out_pw %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab("Fitted values") +
  ylab("Standardized residuals") +
  geom_smooth()

