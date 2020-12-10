##################################################
### Create indicator variables from knots
##################################################

tokyo = tokyo %>%
  mutate(
    i_1 = if_else(year <= 1980, 0, 1),
    i_2 = if_else(year <= 1992, 0, 1)
  )

tokyo


##################################################
### Fit discontinuous piecewise model
##################################################
# Step function
lm.pw = lm(water_use ~ 1 + i_1 + i_2, data = tokyo)





##################################################
### Plot the fitted model
##################################################

tokyo %>%
  mutate(yhat = predict(lm.pw)) %>%
  ggplot(aes(x = year, y = yhat)) +
    geom_line() +
    geom_point(data = tokyo, aes(x = year, y = water_use), alpha = 0.5) +
    theme_bw() +
    scale_x_continuous(name = "Year", breaks = seq(from = 1970, to = 2005, by = 5)) +
    scale_y_continuous(
      name = "Residential water use (in 1000 cubic feet)", 
      breaks = seq(from = 600000, to = 740000, by = 20000)
    )

# Discontinuous linear
lm.pw = lm(water_use ~ 1 + year + i_1 + i_2, data = tokyo)
lm.pw = lm(water_use ~ 1 + year + i_1 + i_2 + i_1:year + i_2:year, data = tokyo)


tidy(lm.pw)

