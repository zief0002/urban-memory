##################################################
### Fit linear spline model with truncated basis functions
##################################################

# Create indicators; knots at {1980, 1992}
tokyo = tokyo %>%
  mutate(
    s_1 = if_else(year <= 1980, 0, year - 1980),
    s_2 = if_else(year <= 1992, 0, year - 1992)
  )

# Fit model
lpw = lm(water_use ~ 1 + year + s_1 + s_2, data = tokyo)
coef(lpw)


# Examine model matrix
model.matrix(lpw)
model.matrix(lpw)[1:3, ] #First 3 cases


# Compute fitted values (Xb)
model.matrix(lpw)[1:3, ] %*% coef(lpw)

#fitted(lpw)


##################################################
### Fit linear spline model with B-spline basis functions
##################################################

bs.1 = lm(water_use ~ 1 + bs(year, knots = c(1980, 1992), degree = 1), data = tokyo)
coef(bs.1)


# Get model matrix
model.matrix(bs.1)
model.matrix(bs.1)[1:3, ] #First 3 cases


# Get fitted values -- these are the same
model.matrix(bs.1)[1:3, ] %*% coef(bs.1)



##################################################
### Understanding the B-spline basis functions
##################################################

# Focus on the first indicator
model.matrix(bs.1)[ , 2]


# Look at plot of first indicator values versus year
plot(x = tokyo$year, y = model.matrix(bs.1)[ , 2], type = "b")


# Look at plot of second indicator values versus year
plot(x = tokyo$year, y = model.matrix(bs.1)[ , 3], type = "b")


# Look at plot of third indicator values versus year
plot(x = tokyo$year, y = model.matrix(bs.1)[ , 4], type = "b")



##################################################
### Fit quadratic spline model with B-spline basis functions
##################################################

bs.2 = lm(water_use ~ 1 + bs(year, knots = c(1980, 1992), degree = 2), data = tokyo)
coef(bs.2)
model.matrix(bs.2)[1:3, ]


# Look at plot of indicator values versus year
plot(x = tokyo$year, y = model.matrix(bs.2)[ , 2], type = "b")
plot(x = tokyo$year, y = model.matrix(bs.2)[ , 3], type = "b")
plot(x = tokyo$year, y = model.matrix(bs.2)[ , 4], type = "b")
plot(x = tokyo$year, y = model.matrix(bs.2)[ , 5], type = "b")


##################################################
### Fit quadratic spline model with B-spline basis functions
##################################################

bs.3 = lm(water_use ~ 1 + bs(year, knots = c(1980, 1992), degree = 3), data = tokyo)
coef(bs.3)
model.matrix(bs.3)[1:3, ]


# Look at plot of indicator values versus year
plot(x = tokyo$year, y = model.matrix(bs.3)[ , 2], type = "b")
plot(x = tokyo$year, y = model.matrix(bs.3)[ , 3], type = "b")
plot(x = tokyo$year, y = model.matrix(bs.3)[ , 4], type = "b")
plot(x = tokyo$year, y = model.matrix(bs.3)[ , 5], type = "b")
plot(x = tokyo$year, y = model.matrix(bs.3)[ , 6], type = "b")

