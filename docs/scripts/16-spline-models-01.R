##################################################
### Load libraries
##################################################

library(broom)
library(tidyverse)



##################################################
### Import data
##################################################

tokyo = read_csv("https://raw.githubusercontent.com/zief0002/epsy-8264/master/data/tokyo-water-use.csv")


##################################################
### Plot data
##################################################

ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  theme_light() 







##################################################
### Create indicator variables from knots
##################################################

# knots at {1980, 1992}

tokyo = tokyo %>%
  mutate(
    s_1 = if_else(year <= 1980, 0, year - 1980),
    s_2 = if_else(year <= 1992, 0, year - 1992)
  )

tokyo



##################################################
### Fit linear piecewise model and plot results
##################################################

lm.linear.pw = lm(water_use ~ 1 + year + s_1 + s_2, data = tokyo) 
tidy(lm.linear.pw)
glance(lm.linear.pw)


# Create plotting data
plot_data = data.frame(year = seq(from = 1973, to = 1999, by = 0.1)) %>%
  mutate(
    s_1 = if_else(year <= 1980, 0, year - 1980),
    s_2 = if_else(year <= 1992, 0, year - 1992)
  ) %>% mutate(
    yhat.1 = predict(lm.linear.pw, newdata = .)
    )


ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  geom_line(data = plot_data, aes(y = yhat.1), color = "red") +
  theme_light()
  


##################################################
### Fit quadratic piecewise model and plot results
##################################################

lm.quad.pw = lm(water_use ~ 1 + year + I(year^2) + s_1 + s_2, data = tokyo) 


# Create plotting data
plot_data = plot_data %>% mutate(
    yhat.2 = predict(lm.quad.pw, newdata = .)
  )


ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  geom_line(data = plot_data, aes(y = yhat.2), color = "red") +
  theme_light()



##################################################
### Fit second quadratic piecewise model and plot results
##################################################

lm.quad.pw.2 = lm(water_use ~ 1 + year + I(year^2) + s_1 + I(s_1^2) + s_2 + I(s_2^2), data = tokyo) 



# Create plotting data
plot_data = plot_data %>% mutate(
  yhat.3 = predict(lm.quad.pw.2, newdata = .)
)


ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  geom_line(data = plot_data, aes(y = yhat.3), color = "red") +
  theme_light()


##################################################
### Check residuals
##################################################

augment(lm.linear.pw) %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_light()


augment(lm.quad.pw) %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_light()


augment(lm.quad.pw.2) %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_light()



##################################################
### Fit cubic piecewise model and plot results
##################################################

lm.cubic.pw = lm(water_use ~ 1 + year + I(year^2) + I(year^3) + s_1 + s_2, data = tokyo) 


# Create plotting data
plot_data = plot_data %>% mutate(
  yhat.4 = predict(lm.cubic.pw, newdata = .)
)


# Check correlations
X = model.matrix(lm.cubic.pw)

cor(X)



##################################################
### Orthogonal polynomial basis
##################################################

lm.cubic.pw = lm(water_use ~ 1 + poly(year, 3) + s_1 + s_2, data = tokyo) 
lm.cubic.pw.2 = lm(water_use ~ 1 + poly(year, 3) + 
                     s_1 + I(s_1^2) + I(s_1^3) + 
                     s_2 + I(s_2^2) + I(s_2^3), data = tokyo) 


# Check correlations
X = model.matrix(lm.cubic.pw)
cor(X)


plot_data = plot_data %>% mutate(
  yhat.4 = predict(lm.cubic.pw.2, newdata = .)
)

ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  geom_line(data = plot_data, aes(y = yhat.4), color = "red") +
  theme_light()







