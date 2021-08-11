##################################################
### Load libraries
##################################################

library(broom)
library(tidyverse)
library(splines)



##################################################
### Import and prepare data
##################################################

tokyo = read_csv("data/tokyo-water-use.csv")
head(tokyo)



##################################################
### Spline model
##################################################

# Create indicators
tokyo = tokyo %>%
  mutate(
    I_1 = if_else(year <= 1980, 0, (year - 1980)^3),
    I_2 = if_else(year <= 1992, 0, (year - 1992)^3)
  )


# Fit spline model
lm.spline.3 = lm(water_use ~ 1 + poly(year, 3) + I_1 + I_2, data = tokyo)
glance(lm.spline.3)



##################################################
### Plot the fitted model
##################################################

# Create plotting data
plot_data = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) %>%
  mutate(
    I_1 = if_else(year <= 1980, 0, (year - 1980)^3),
    I_2 = if_else(year <= 1992, 0, (year - 1992)^3)
  ) %>%
  mutate(
    yhat = predict(lm.spline.3, newdata = .)
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
### B-spline model
##################################################

# Fit cubic spline using B-spline basis - Need to load the splines package
bs.1 = lm(water_use ~ 1 + bs(year, knots = c(1980, 1992)), data = tokyo)

# Model-level output
glance(bs.1)

# Coefficeint-level output
tidy(bs.1)



##################################################
### Design matrices to compare basis
##################################################

# First 10 rows of design matrix for initial basis---using poly()
model.matrix(lm.spline.3)[1:10, ]

# First 10 rows of design matrix for B-spline basis
model.matrix(bs.1)[1:10, ]



##################################################
### Plot fitted B-spline model
##################################################

# Set up data to plot
plot_data = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) %>%
  mutate(
    yhat = predict(bs.1, newdata = .)
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
### Plot fitted B-spline model using geom_smooth()
##################################################

# Plot
ggplot(data = plot_data, aes(x = year, y = yhat)) +
  geom_smooth(method = "lm", formula = y~bs(x, knots = c(1980, 1992)), color = "red") +
  geom_point(data = tokyo, aes(x = year, y = water_use), alpha = 0.5) +
  theme_light() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) + 
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)",
    breaks = seq(from = 600000, to = 740000, by = 20000)
  )





