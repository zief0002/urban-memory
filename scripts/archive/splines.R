library(splines)

# Predictions and SEs
# https://educationalresearchtechniques.com/2018/01/01/polynomial-spline-regression-in-r/


x = seq(from = 0, to = 1, by = 0.2)
x

bs(x, knots = 6)
ns(x)
?ns

matrix(interpSpline(x, knots = 6))

matrix(ns(x, knots = 2))


ispl = polySpline(interpSpline( weight ~ height, women, bSpline = TRUE))

print( ispl )   # print the piecewise polynomial representation
plot( ispl )    # plots over the range of the knots
points( women$height, women$weight )

predict(ispl)

#http://www.startribune.com/a-deeper-look-at-minneapolis-crime-rates/493163741/
mpls = readr::read_csv("~/Dropbox/epsy-8264/data/mpls-violent-crime.csv")

tokyo = readr::read_csv("~/Dropbox/epsy-8264/data/tokyo-water-use.csv")

# Polynomial regression fit
ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  scale_x_continuous(name = "Year", limits = c(1971, 2001)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 3))



# Polynomial regression fit
ggplot(data = mpls, aes(x = year, y = crime_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 4))




# B-spline
library(splines)
library(broom)

b_spline = lm(crime_rate ~ bs(year, knots = c(2001, 2006)), data = mpls)
b_spline = lm(water_use ~ bs(year, knots = c(1980, 1992), degree = 1), data = tokyo)
tidy(b_spline)



length(b_spline$coefficients)
b_spline$rank


plot_data = data.frame(
  year = 1973:1999
) %>%
  mutate(
    y_hat = predict(b_spline, newdata = .)
  )


# Polynomial regression fit
ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  scale_x_continuous(name = "Year", limits = c(1971, 2001)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 3)) + 
  geom_line(data = plot_data, aes(x = year, y = y_hat), color = "red") 

ggplot(data = mpls, aes(x = year, y = crime_rate)) +
  geom_point() +
  geom_line(data = plot_data, aes(x = year, y = y_hat))  +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 4))


# Natural spline
nat_spline = lm(crime_rate ~ ns(year, knots = 3), data = mpls)
nat_spline = lm(water_use ~ ns(year, knots = c(1980, 1992), intercept = TRUE), data = tokyo)

plot_data2 = data.frame(
  year = 1973:1999
) %>%
  mutate(
    y_hat = predict(nat_spline, newdata = ., se.fit = TRUE)
  )


ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  scale_x_continuous(name = "Year", limits = c(1971, 2001)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 3)) + 
  geom_line(data = plot_data, aes(x = year, y = y_hat), color = "red") +
  geom_line(data = plot_data2, aes(x = year, y = y_hat), color = "green")


### Three different plots

p1 = ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  scale_x_continuous(name = "Year", limits = c(1971, 2001)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 3)) +
  ggtitle("3rd-Degree Polynomial Regression")

p2 = ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  scale_x_continuous(name = "Year", limits = c(1971, 2001)) +
  geom_line(data = plot_data2, aes(x = year, y = y_hat), color = "red") +
  ggtitle("Natural-Spline")

p3 = ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  scale_x_continuous(name = "Year", limits = c(1971, 2001)) +
  geom_line(data = plot_data, aes(x = year, y = y_hat), color = "blue") +
  ggtitle("B-Spline")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

lm_poly = lm(water_use ~ 1 + year + I(year^2) + I(year^3), data = tokyo)

data.frame(year = c(2001, 2015)) %>%
  mutate(
    poly_reg = predict(lm_poly, newdata = .),
    nat_spline = predict(nat_spline, newdata = .),
    b_spline = predict(b_spline, newdata = .)
  )




ggplot(data = mpls, aes(x = year, y = crime_rate)) +
  geom_point() +
  geom_line(data = plot_data, aes(x = year, y = y_hat))  +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 4)) +
  geom_line(data = plot_data2, aes(x = year, y = y_hat), color = "blue")


interpolate_spline = interpSpline(mpls$year, mpls$crime_rate)

plot_data3 = data.frame(
  year = seq(from = 2000, to = 2017, length.out = 18)
)

my_predictions = data.frame(
   x = predict(interpolate_spline, newdata = plot_data3)$x,
   y_hat = predict(interpolate_spline, newdata = plot_data3)$y
  )

ggplot(data = mpls, aes(x = year, y = crime_rate)) +
  geom_point() +
  geom_line(data = plot_data, aes(x = year, y = y_hat))  +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 4)) +
  geom_line(data = plot_data2, aes(x = year, y = y_hat), color = "red") +
  geom_line(data = my_predictions, aes(x = x, y = y_hat), color = "purple")


plot(crime_rate ~ year, data = mpls)
curve(crime_rate ~ bs(x), data = mpls)



# http://rstudio-pubs-static.s3.amazonaws.com/11323_9d5a69cc3c784d3680b2f7243fd29370.html

x = runif(50, min = 0, max = 10)
y_hat = 1/5 * x + cos(x + 1)
e = rnorm(50, mean = 0, sd = 0.5)
y = y_hat + e

fake = data.frame(
  x, y
)

# ggplot(data = fake, aes(x = x, y = y)) +
#   geom_point() +
#   theme_light() +
#   stat_function(fun = function(x) 1/5 * x + cos(x + 1), geom = "line", linetype = "dashed") +
#   scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
#   theme(
#     panel.grid = element_line(NULL)
#   )

fake_1 = fake %>%
  filter(x <= 3.5)

fake_2 = fake %>%
  filter(x > 3.5 & x <= 7)

fake_3 = fake %>%
  filter(x > 7)


ggplot(data = fake, aes(x = x, y = y)) +
  geom_point() +
  theme_bw() +
  stat_function(fun = function(x) 1/5 * x + cos(x + 1), geom = "line", linetype = "dashed") +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  geom_vline(xintercept = c(3.5, 7), linetype = "dotted") +
  geom_smooth(data = fake_1, method = "lm", se = FALSE, formula = y~1+x) + 
  geom_smooth(data = fake_2, method = "lm", se = FALSE, formula = y~1+x) + 
  geom_smooth(data = fake_3, method = "lm", se = FALSE, formula = y~1+x)


ggplot(data = fake, aes(x = x, y = y)) +
  geom_point() +
  theme_bw() +
  stat_function(fun = function(x) 1/5 * x + cos(x + 1), geom = "line", linetype = "dashed") +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  geom_vline(xintercept = c(3.5, 7), linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, formula = y~poly(x, 4))  +
  geom_smooth(method = "lm", se = FALSE, formula = y~1+bs(x, df = 10), color = "green") +
  geom_smooth(method = "lm", se = FALSE, formula = y~1+ns(x, knots = c(3.5, 7)), color = "purple")





tokyo_1 = tokyo %>%
  filter(year <= 1980)

tokyo_2 = tokyo %>%
  filter(year > 1980 & year <= 1992)

tokyo_3 = tokyo %>%
  filter(year > 1992)

ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits = c(1972, 2001), breaks = seq(from = 1975, to = 2000, by = 5)) +
  scale_y_continuous(limits = c(600000, 750000), breaks = seq(from = 600000, to = 750000, by = 50000)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  geom_vline(xintercept = c(1980, 1992), linetype = "dotted") +
  geom_smooth(data = tokyo_1, method = "lm", se = FALSE, formula = y~1) + 
  geom_smooth(data = tokyo_2, method = "lm", se = FALSE, formula = y~1) + 
  geom_smooth(data = tokyo_3, method = "lm", se = FALSE, formula = y~1)


ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits = c(1972, 2001), breaks = seq(from = 1975, to = 2000, by = 5)) +
  scale_y_continuous(limits = c(600000, 750000), breaks = seq(from = 600000, to = 750000, by = 50000)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  geom_vline(xintercept = c(1980, 1992), linetype = "dotted") +
  geom_smooth(data = tokyo_1, method = "lm", se = FALSE, formula = y~1 + x) + 
  geom_smooth(data = tokyo_2, method = "lm", se = FALSE, formula = y~1 + x) + 
  geom_smooth(data = tokyo_3, method = "lm", se = FALSE, formula = y~1 + x)

ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits = c(1972, 2001), breaks = seq(from = 1975, to = 2000, by = 5)) +
  scale_y_continuous(limits = c(600000, 750000), breaks = seq(from = 600000, to = 750000, by = 50000)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  geom_vline(xintercept = c(1980, 1992), linetype = "dotted") +
  geom_smooth(data = tokyo_1, method = "lm", se = FALSE, formula = y~1+poly(x, 3)) + 
  geom_smooth(data = tokyo_2, method = "lm", se = FALSE, formula = y~1+poly(x, 3)) + 
  geom_smooth(data = tokyo_3, method = "lm", se = FALSE, formula = y~1+poly(x, 3))


tokyo = tokyo %>%
  mutate(
    I_1980 = if_else(year > 1980, 1, 0),
    I_1992 = if_else(year > 1992, 1, 0),
    pw_1980 = (year - 1980) * I_1980,
    pw_1992 = (year - 1992) * I_1992
  )

lm.pw = lm(water_use ~ 1 + year + pw_1980 + pw_1992, data = tokyo)

ggplot(data = tokyo, aes(x = year, y = water_use)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits = c(1972, 2001), breaks = seq(from = 1975, to = 2000, by = 5)) +
  scale_y_continuous(limits = c(600000, 750000), breaks = seq(from = 600000, to = 750000, by = 50000)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  geom_vline(xintercept = c(1980, 1992), linetype = "dotted") +
  geom_line(data = augment(lm.pw), aes(x = year, y = .fitted))  +
  geom_smooth(data = tokyo_1, method = "lm", se = FALSE, formula = y~1 + x) + 
  geom_smooth(data = tokyo_2, method = "lm", se = FALSE, formula = y~1 + x) + 
  geom_smooth(data = tokyo_3, method = "lm", se = FALSE, formula = y~1 + x)



########################################

mpls = mpls %>%
  mutate(
    I_2005 = if_else(year > 2005 & year <= 2011, 1, 0),
    I_2011 = if_else(year > 2011, 1, 0)
  )

lm.pw.0 = lm(crime_rate ~ 1 + I((year - 2005)*I_2005) + I((year - 2011)*I_2011), data = mpls)
tidy(lm.pw.0)


ggplot(data = mpls, aes(x = year, y = crime_rate)) +
  geom_point() +
  theme_bw()  +
  scale_x_continuous(limits = c(2000, 2017), breaks = seq(from = 2000, to = 2015, by = 5)) +
  #scale_y_continuous(limits = c(600000, 750000), breaks = seq(from = 600000, to = 750000, by = 50000)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  geom_vline(xintercept = c(2005, 2011), linetype = "dotted") +
  geom_smooth(data = mpls_1, method = "lm", se = FALSE, formula = y~1) + 
  geom_smooth(data = mpls_2, method = "lm", se = FALSE, formula = y~1) + 
  geom_smooth(data = mpls_3, method = "lm", se = FALSE, formula = y~1)



lm.pw = lm(crime_rate ~ 1 + year + I((year - 2005)*I_2005) + I((year - 2011)*I_2011), data = mpls)

mpls_1 = mpls %>%
  filter(year <= 2005)

mpls_2 = mpls %>%
  filter(year > 2005 & year <= 2011)

mpls_3 = mpls %>%
  filter(year > 2011)

ggplot(data = mpls, aes(x = year, y = crime_rate)) +
  geom_point() +
  theme_bw()  +
  scale_x_continuous(limits = c(2000, 2017), breaks = seq(from = 2000, to = 2015, by = 5)) +
  #scale_y_continuous(limits = c(600000, 750000), breaks = seq(from = 600000, to = 750000, by = 50000)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  geom_vline(xintercept = c(2005, 2011), linetype = "dotted") +
  geom_smooth(data = mpls_1, method = "lm", se = FALSE, formula = y~1 + x) + 
  geom_smooth(data = mpls_2, method = "lm", se = FALSE, formula = y~1 + x) + 
  geom_smooth(data = mpls_3, method = "lm", se = FALSE, formula = y~1 + x) +
  geom_line(data = augment(lm.pw), aes(x = year, y = .fitted), color = "red")


lm.pw.3 = lm(crime_rate ~ 1 + year + I(year^2) + I(year^3) + 
               I((year - 2005)*I_2005) + I((year - 2011)*I_2011) + 
               I((year - 2005)^2*I_2005) + I((year - 2011)^2*I_2011) +
               I((year - 2005)^3*I_2005) + I((year - 2011)^3*I_2011), data = mpls)


ggplot(data = mpls, aes(x = year, y = crime_rate)) +
  geom_point() +
  theme_bw()  +
  scale_x_continuous(limits = c(2000, 2017), breaks = seq(from = 2000, to = 2015, by = 5)) +
  #scale_y_continuous(limits = c(600000, 750000), breaks = seq(from = 600000, to = 750000, by = 50000)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  geom_vline(xintercept = c(2005, 2011), linetype = "dotted") +
  geom_smooth(data = mpls_1, method = "lm", se = FALSE, formula = y~1+poly(x,3)) + 
  geom_smooth(data = mpls_2, method = "lm", se = FALSE, formula = y~1+poly(x,3)) + 
  geom_smooth(data = mpls_3, method = "lm", se = FALSE, formula = y~1+poly(x,3)) +
  geom_line(data = augment(lm.pw.3), aes(x = year, y = .fitted), color = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y~1+bs(x, knots = c(2005, 2011)), color = "green") +
  geom_smooth(method = "lm", se = FALSE, formula = y~1+ns(x, knots = c(2005, 2011)), color = "purple") +
  geom_smooth(method = "lm", se = FALSE, formula = y~1+poly(x, 3), color = "skyblue")



gam.1 = mgcv::gam(crime_rate ~ 1 + s(year), data = mpls)
gam.2 = mgcv::gam(crime_rate ~ 1 + s(year, k = 5), data = mpls)

ns.1 = ns(mpls$year, df = 10)

ggplot(data = mpls, aes(x = year, y = crime_rate)) +
  geom_point() +
  theme_bw()  +
  scale_x_continuous(limits = c(2000, 2017), breaks = seq(from = 2000, to = 2015, by = 5)) +
  #scale_y_continuous(limits = c(600000, 750000), breaks = seq(from = 600000, to = 750000, by = 50000)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  geom_vline(xintercept = c(2005, 2011), linetype = "dotted") +
  geom_smooth(data = mpls_1, method = "lm", se = FALSE, formula = y~1+poly(x,3)) + 
  geom_smooth(data = mpls_2, method = "lm", se = FALSE, formula = y~1+poly(x,3)) + 
  geom_smooth(data = mpls_3, method = "lm", se = FALSE, formula = y~1+poly(x,3)) +
  geom_line(data = augment(gam.2), aes(x = year, y = .fitted), color = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y~1+bs(x, df = 10)) +
  geom_vline(xintercept = as.numeric(attr(ns.1, "knots")))



#########################

ns.1 = ns(mc$times, df = 9)

ggplot(data = mc, aes(x = times, y = accel)) +
  geom_point() +
  theme_bw()  +
  scale_x_continuous(limits = c(0, 50), breaks = seq(from = 0, to = 50, by = 5)) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  geom_vline(xintercept = c(2005, 2011), linetype = "dotted") + 
  #geom_smooth(method = "lm", se = FALSE, formula = y~1+poly(x, 8), color = "skyblue")  +
  #geom_smooth(method = "lm", se = FALSE, formula = y~1+bs(x, df = 6), color = "green") +
  geom_smooth(method = "lm", se = TRUE, formula = y~1+bs(x, df = 3), color = "purple") +
  geom_smooth(method = "gam", formula = y~1+s(x, bs = "cr"), color = "green")
#geom_smooth(method = "lm", se = FALSE, formula = y~1+ns(x, df = 3)) +
  #geom_vline(xintercept = as.numeric(attr(b.3, "knots")))

ns.5 = lm(accel ~ 1 + ns(times, df = 6), data = mc)
ns.4 = lm(accel ~ 1 + ns(times, df = 5), data = mc)
ns.6 = lm(accel ~ 1 + ns(times, df = 9), data = mc)
AIC(ns.5)
AIC(ns.4)
AIC(ns.6)


n.3 = ns(mc$times, df = 3)
b.3 = bs(mc$times, df = 3, degree = 1)
ns.3 = lm(accel ~ 1 + ns(times, df = 3), data = mc)
tidy(ns.3)

bs.3 = lm(accel ~ 1 + bs(times, df = 3), data = mc)
tidy(bs.3)

library(mgcv)
tidy(gam(accel ~ 1 + s(times, bs = "cr"), data = mc), parametric = TRUE)
summary(gam(accel ~ 1 + s(times, bs = "cr"), data = mc))
?s

matrix(ns(mc$times, df = 2))

########## ASSIGNMENT -- MPLS

mpls = readr::read_csv("~/Dropbox/epsy-8264/data/mpls-violent-crime.csv")


ggplot(data = mpls, aes(x = year, y = crime_rate)) +
  geom_point()  +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ bs(x, df = 5)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ bs(x, df = 6), color = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ bs(x, df = 7), color = "green")




mcycle = readr::read_csv("~/Dropbox/epsy-8264/data/mcycle.csv")


ggplot(data = mcycle, aes(x = times, y = accel)) +
  geom_point()  +
  #geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 10)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ bs(x, knots = c(10, 20, 30, 40, 50))) +
  scale_x_continuous(breaks = 0:60)


# Divide data into 10 folds
library(modelr)
library(purrr)
set.seed(100)

my_cv = mcycle %>%
  crossv_kfold(k = 5)



cv_1 = my_cv %>% mutate(model = map(train, ~lm(accel ~ 1 + bs(times, df = 10), data = .)), MSE = map2_dbl(model, test, modelr::mse), knots = 7)
cv_2 = my_cv %>% mutate(model = map(train, ~lm(accel ~ 1 + bs(times, df = 11), data = .)), MSE = map2_dbl(model, test, modelr::mse), knots = 8)
cv_3 = my_cv %>% mutate(model = map(train, ~lm(accel ~ 1 + bs(times, df = 12), data = .)), MSE = map2_dbl(model, test, modelr::mse), knots = 9)
cv_4 = my_cv %>% mutate(model = map(train, ~lm(accel ~ 1 + bs(times, df = 9), data = .)), MSE = map2_dbl(model, test, modelr::mse), knots = 6)
cv_5 = my_cv %>% mutate(model = map(train, ~lm(accel ~ 1 + bs(times, df = 8), data = .)), MSE = map2_dbl(model, test, modelr::mse), knots = 5)
cv_6 = my_cv %>% mutate(model = map(train, ~lm(accel ~ 1 + bs(times, df = 7), data = .)), MSE = map2_dbl(model, test, modelr::mse), knots = 4)




# Evaluate CV-MSE
rbind(cv_1, cv_2, cv_3, cv_4, cv_5, cv_6) %>%
  group_by(knots) %>%
  summarize(
    cv_mse = mean(MSE)
  ) %>%
  arrange(cv_mse)


geom_smooth(method = "lm", se = FALSE, formula = y ~ bs(x, df = 5)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ bs(x, df = 6), color = "red") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ bs(x, df = 7), color = "green")
