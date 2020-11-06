##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)
library(splines)
library(tidyr)



##################################################
### Import and prepare data
##################################################

tokyo = read_csv("~/Dropbox/epsy-8264/data/tokyo-water-use.csv")
head(tokyo)



##################################################
### Piecewise models
##################################################

# Create indicators
tokyo = tokyo %>%
  mutate(
    I_1 = if_else(year <= 1980, 0, year - 1980),
    I_2 = if_else(year <= 1992, 0, year - 1992)
  )


# Fit linear piecewise model
lm.pw.1 = lm(water_use ~ 1 + year + I_1 + I_2, data = tokyo)


# Fit quadratic piecewise model
lm.pw.2 = lm(water_use ~ 1 + year + I(year^2) + I_1 + I(I_1^2) + I_2 + I(I_2^2), data = tokyo)



##################################################
### Plot fitted models
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
    yhat1 = predict(lm.pw.1, newdata = plot_data),
    yhat2 = predict(lm.pw.2, newdata = plot_data)
  ) %>%
  gather(model, yhat, yhat1:yhat2) %>%
  mutate(
    model = factor(model, levels = c("yhat1", "yhat2"), labels = c("Linear", "Quadratic"))
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
  ) +
  facet_wrap(~model)



##################################################
### Examine residuals
##################################################

# Augment the model
out_pw_1 = augment(lm.pw.1)
out_pw_2 = augment(lm.pw.2)


# Evalate residuals: Linear piecewise model
sm.density(out_pw_1$.std.resid, model = "normal", xlab = "Standardized residuals from Linear model")

out_pw_1 %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
    geom_point()  +
    geom_smooth() +
    geom_hline(yintercept = 0) +
    theme_bw() +
    xlab("Fitted values") +
    ylab("Standardized residuals") +
    ggtitle("Linear")


# Evalate residuals: Quadratic piecewise model
sm.density(out_pw_2$.std.resid, model = "normal", xlab = "Standardized residuals from Quadratic model")

out_pw_2 %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
    geom_point()  +
    geom_smooth() +
    geom_hline(yintercept = 0) +
    theme_bw() +
    xlab("Fitted values") +
    ylab("Standardized residuals") +
    ggtitle("Quadratic")




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

# Fit cubic spline using B-spline basis
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
### Uniform knots
##################################################

# Fit cubic spline using B-spline basis with two uniformly distributed interior knots
bs.2 = lm(water_use ~ 1 + bs(year, df = 5), data = tokyo)


# Set up plot data
plot_data2 = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) %>%
  mutate(
    yhat = predict(bs.2, newdata = .)
  )


# Plot fitted lines for both spline models
ggplot(data = plot_data, aes(x = year, y = yhat)) +
  geom_line() +
  geom_point(data = tokyo, aes(x = year, y = water_use), alpha = 0.5) +
  theme_bw() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
  ) +
  geom_line(data = plot_data2, color = "red")



##################################################
### How many knots?
##################################################

# Fit cubic spline using B-spline basis with two uniformly distributed interior knots
bs.2 = lm(water_use ~ 1 + bs(year, df = 5), data = tokyo)
bs.3 = lm(water_use ~ 1 + bs(year, df = 6), data = tokyo)
bs.4 = lm(water_use ~ 1 + bs(year, df = 7), data = tokyo)
bs.5 = lm(water_use ~ 1 + bs(year, df = 8), data = tokyo)

# Set up plot data
plot_data = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) %>%
  mutate(
    yhat = predict(bs.1, newdata = .),
    model = "2 knots (1980, 1992)"
  ) 

plot_data2 = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) %>%
  mutate(
    yhat = predict(bs.2, newdata = .),
    model = "2 knots (df=5)"
  ) 

plot_data3 = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) %>%
  mutate(
    yhat = predict(bs.3, newdata = .),
    model = "3 knots (df=6)"
  ) 

plot_data4 = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) %>%
  mutate(
    yhat = predict(bs.4, newdata = .),
    model = "4 knots (df=7)"
  ) 

plot_data5 = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) %>%
  mutate(
    yhat = predict(bs.5, newdata = .),
    model = "5 knots (df=8)"
  ) 

# Combine into one dataset
combined_data = rbind(plot_data, plot_data2, plot_data3, plot_data4, plot_data5)

# Plot fitted lines for both spline models
ggplot(data = combined_data, aes(x = year, y = yhat)) +
  geom_line(aes(color = model)) +
  geom_point(data = tokyo, aes(x = year, y = water_use), alpha = 0.5) +
  theme_bw() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
  ) +
  scale_color_brewer(name = "Model", palette = "Set1")



##################################################
### Use AICc to determine number of knots
##################################################

library(AICcmodavg)

aictab(
  cand.set = list(bs.2, bs.3, bs.4, bs.5), 
  modnames = c("2 knots (df=5)", "3 knots (df=6)", "4 knots (df=7)", "5 knots (df=8)")
)



##################################################
### Examine residuals from two "best" models
##################################################

# Augment the model
out_bs_2 = data.frame(.fitted = fitted(bs.2), .std.resid = rstandard(bs.2))
out_bs_3 = data.frame(.fitted = fitted(bs.3), .std.resid = rstandard(bs.3))

# Two knot model
sm.density(out_bs_2$.std.resid, model = "normal", xlab = "Standardized residuals from model with 2 knots (df=5)")

out_bs_2 %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
    geom_point()  +
    geom_smooth() +
    geom_hline(yintercept = 0) +
    theme_bw() +
    xlab("Fitted values") +
    ylab("Standardized residuals") +
    ggtitle("Model with 2 knots (df=5)")

# Three knot model
sm.density(out_bs_3$.std.resid, model = "normal", xlab = "Standardized residuals from model with 3 knots (df=6)")

out_bs_3 %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
    geom_point()  +
    geom_smooth() +
    geom_hline(yintercept = 0) +
    theme_bw() +
    xlab("Fitted values") +
    ylab("Standardized residuals") +
    ggtitle("Model with 3 knots (df=6)")


##################################################
### Where were knots placed?
##################################################

attr(bs.2$terms, "predvars")


##################################################
### Set uniform knots and use to fit model
##################################################

# Get knot placement
my_knots = quantile(tokyo$year, probs = c(1/3, 2/3))
my_knots


# Fit model
bs.2.2 = lm(water_use ~ 1 + bs(year, knots = my_knots), data = tokyo)



##################################################
### Graphical uncertainty
##################################################

# Set up data for plotting
plot_data = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) 

# Compute fitted values and SEs
preds = predict(bs.2, newdata = plot_data, se.fit = TRUE)

# Add the fitted values, SEs, and limits to the plotting data
plot_data = plot_data %>%
  mutate(
    yhat = preds$fit,
    se = preds$se.fit,
    lower_limit = yhat - 2*se,
    upper_limit = yhat + 2*se
  )

# Plot
ggplot(data = plot_data, aes(x = year, y = yhat)) +
  geom_line(color = "red") +
  geom_line(aes(y = lower_limit), color = "red", linetype = "dashed") +
  geom_line(aes(y = upper_limit), color = "red", linetype = "dashed") +
  geom_point(data = tokyo, aes(x = year, y = water_use), alpha = 0.5) +
  theme_bw() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
  )



##################################################
### Natural splines
##################################################

# Fit natural spline model with two knots
ns.2 = lm(water_use ~ 1 + ns(year, df = 3), data = tokyo)

# Obtain knot locations
attr(ns.2$terms, "predvars")



##################################################
### Plot fitted natural spline model
##################################################

# Set up plotting data
plot_data_ns = data.frame(
  year = seq(from = 1973, to = 1999, by = 0.1)
) 

# Compute fitted values and SEs
preds_ns = predict(ns.2, newdata = plot_data_ns, se.fit = TRUE)

# Add the fitted values, SEs, and limits to the plotting data
plot_data_ns = plot_data_ns %>%
  mutate(
    yhat = preds_ns$fit,
    se = preds_ns$se.fit,
    lower_limit = yhat - 2*se,
    upper_limit = yhat + 2*se
  )

# Plot
ggplot(data = plot_data_ns, aes(x = year, y = yhat)) +
  geom_line(color = "blue") +
  geom_line(aes(y = lower_limit), color = "blue", linetype = "dashed") +
  geom_line(aes(y = upper_limit), color = "blue", linetype = "dashed") +
  geom_point(data = tokyo, aes(x = year, y = water_use), alpha = 0.5) +
  theme_bw() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
  )



##################################################
### Extrapolate
##################################################

# Set up data for plotting
plot_data_bs = data.frame(
  year = seq(from = 1970, to = 2005, by = 0.1)
) 

# Compute fitted values and SEs
preds_bs = predict(bs.2, newdata = plot_data_bs, se.fit = TRUE)

# Add the fitted values, SEs, and limits to the plotting data
plot_data_bs = plot_data_bs %>%
  mutate(
    yhat = preds_bs$fit,
    se = preds_bs$se.fit,
    lower_limit = yhat - 2*se,
    upper_limit = yhat + 2*se
  )


plot_data_ns = data.frame(
  year = seq(from = 1970, to = 2005, by = 0.1)
) 

# Compute fitted values and SEs
preds_ns = predict(ns.2, newdata = plot_data_ns, se.fit = TRUE)

# Add the fitted values, SEs, and limits to the plotting data
plot_data_ns = plot_data_ns %>%
  mutate(
    yhat = preds_ns$fit,
    se = preds_ns$se.fit,
    lower_limit = yhat - 2*se,
    upper_limit = yhat + 2*se
  )

# Plot
ggplot(data = plot_data_bs, aes(x = year, y = yhat)) +
  geom_line(color = "red") +
  geom_line(aes(y = lower_limit), color = "red", linetype = "dashed") +
  geom_line(aes(y = upper_limit), color = "red", linetype = "dashed") +
  geom_line(data = plot_data_ns, color = "blue") +
  geom_line(data = plot_data_ns, aes(y = lower_limit), color = "blue", linetype = "dashed") +
  geom_line(data = plot_data_ns, aes(y = upper_limit), color = "blue", linetype = "dashed") +
  geom_point(data = tokyo, aes(x = year, y = water_use), alpha = 0.5) +
  theme_bw() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 400000, to = 900000, by = 50000)
  )


