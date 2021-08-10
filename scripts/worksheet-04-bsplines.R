# Import data
tokyo = read_csv("data/tokyo-water-use.csv")

# Fit cubic spline using B-spline basis
bs.1 = lm(water_use ~ 1 + bs(year, knots = c(1980, 1992)), data = tokyo)

# Create plotting data
plot_data = data.frame(year = 1973:1999) %>%
  mutate(yhat = predict(bs.1, newdata = .))

# Plot 1
ggplot(data = plot_data, aes(x = year, y = yhat)) +
  geom_line() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
  ) +
  theme_light()
  

# Plot
ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, formula = y~bs(x, knots = c(1980, 1992)), size = 0.5) +
  #geom_smooth(method = "lm", se = TRUE, formula = y~bs(x, knots = c(1980, 1992)), size = 0.5) +
  theme_light() +
  scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
  scale_y_continuous(
    name = "Residential water use (in 1000 cubic feet)", 
    breaks = seq(from = 600000, to = 740000, by = 20000)
  ) 


# Predict

# Create plotting data
plot_data = data.frame(year = 1970:2010) 
fits = predict(bs.1, newdata = plot_data, se = TRUE)

plot_data = plot_data %>%
  mutate(
    yhat = fits$fit,
    se = fits$se.fit,
    ll = yhat - 2*se,
    ul = yhat + 2*se
    ) 

# Plot 1
ggplot(data = plot_data, aes(x = year, y = yhat)) +
  geom_ribbon(aes(ymin = ll, ymax = ul), fill = "grey") +
  geom_line() +
  xlab("Year") +
  ylab("Residential water use (in 1000 cubic feet)") +
  theme_light()


plot_data %>%
  mutate(
    yhat = predict(bs.1, newdata = ., se.fit = TRUE)$fit,
    se = predict(bs.1, newdata = ., se.fit = TRUE)$se.fit
  )
    